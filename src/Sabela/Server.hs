{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Sabela.Server
  ( mkApp
  , initState
  ) where

import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_, modifyMVar)
import Control.Concurrent.STM (newBroadcastTChanIO, dupTChan, readTChan, atomically)
import Control.Monad (forever, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, sort)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (status200, hContentType)
import Network.Wai (Application, responseStream)
import Servant
import System.Directory (listDirectory, doesDirectoryExist, makeAbsolute,
                         canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>), makeRelative, takeDirectory)

import Sabela.Markdown (Segment(..), parseMarkdown, reassemble)
import Sabela.Reactive (ReactiveNotebook(..))
import Sabela.Session (queryComplete, queryType, queryInfo, queryDoc)
import Sabela.Server.Types

import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- ── API types ────────────────────────────────────────────────────

type JsonAPI =
       "api" :> "notebook"  :> Get  '[JSON] Notebook
  :<|> "api" :> "load"      :> ReqBody '[JSON] LoadRequest    :> Post '[JSON] Notebook
  :<|> "api" :> "save"      :> ReqBody '[JSON] SaveRequest    :> Post '[JSON] Notebook
  :<|> "api" :> "cell"      :> Capture "id" Int :> ReqBody '[JSON] UpdateCell :> Put '[JSON] Cell
  :<|> "api" :> "cell"      :> ReqBody '[JSON] InsertCell     :> Post '[JSON] Cell
  :<|> "api" :> "cell"      :> Capture "id" Int               :> Delete '[JSON] Notebook
  :<|> "api" :> "run"       :> Capture "id" Int               :> Post '[JSON] RunResult
  :<|> "api" :> "run-all"   :> Post '[JSON] RunAllResult
  :<|> "api" :> "reset"     :> Post '[JSON] Notebook
  -- File explorer
  :<|> "api" :> "files"     :> QueryParam "path" Text         :> Get '[JSON] [FileEntry]
  :<|> "api" :> "file"      :> QueryParam "path" Text         :> Get '[JSON] Text
  :<|> "api" :> "file" :> "create" :> ReqBody '[JSON] CreateFileRequest :> Post '[JSON] FileEntry
  :<|> "api" :> "file" :> "write"  :> ReqBody '[JSON] WriteFileRequest  :> Post '[JSON] Text
  -- IDE
  :<|> "api" :> "complete"  :> ReqBody '[JSON] CompleteRequest :> Post '[JSON] CompleteResult
  :<|> "api" :> "info"      :> ReqBody '[JSON] InfoRequest     :> Post '[JSON] InfoResult
  -- Examples
  :<|> "api" :> "examples"  :> Get '[JSON] [Example]

type FullAPI = JsonAPI
          :<|> "api" :> "events" :> Raw
          :<|> Raw

fullProxy :: Proxy FullAPI
fullProxy = Proxy

-- ── Application wiring ───────────────────────────────────────────

mkApp :: AppState -> ReactiveNotebook -> FilePath -> Application
mkApp st rn staticDir = serve fullProxy (server st rn staticDir)

server :: AppState -> ReactiveNotebook -> FilePath -> Server FullAPI
server st rn staticDir =
  (    getNotebookH st
  :<|> loadNotebookH st rn
  :<|> saveNotebookH st
  :<|> updateCellH st rn
  :<|> insertCellH st
  :<|> deleteCellH st
  :<|> runCellH rn
  :<|> runAllH rn
  :<|> resetH rn st
  :<|> listFilesH st
  :<|> readFileH st
  :<|> createFileH st
  :<|> writeFileH st
  :<|> completeH st
  :<|> infoH st
  :<|> examplesH
  ) :<|> Tagged (sseApp st)
    :<|> serveDirectoryWebApp staticDir

initState :: FilePath -> IO AppState
initState workDir = do
  nb       <- newMVar (Notebook "Untitled" [])
  sess     <- newMVar Nothing
  tmpBase  <- getCanonicalTemporaryDirectory
  tmpDir   <- createTempDirectory tmpBase "sabela-server"
  envRef   <- newIORef Nothing
  nextId   <- newIORef 0
  instDeps <- newIORef Set.empty
  instExts <- newIORef Set.empty
  bcast    <- newBroadcastTChanIO
  gen      <- newIORef 0
  debounce <- newMVar Nothing
  absWork  <- makeAbsolute workDir
  pure AppState
    { stNotebook      = nb
    , stSession       = sess
    , stTmpDir        = tmpDir
    , stWorkDir       = absWork
    , stEnvFile       = envRef
    , stNextId        = nextId
    , stInstalledDeps = instDeps
    , stInstalledExts = instExts
    , stBroadcast     = bcast
    , stGeneration    = gen
    , stDebounceRef   = debounce
    }

-- ── SSE ──────────────────────────────────────────────────────────

sseApp :: AppState -> Application
sseApp st _req respond = do
  chan <- atomically $ dupTChan (stBroadcast st)
  respond $ responseStream status200 hdrs $ \write flush -> do
    write (Builder.byteString ": connected\n\n")
    flush
    forever $ do
      ev <- atomically $ readTChan chan
      let json = LBS.toStrict (encode ev)
      write (Builder.byteString $ "data: " <> json <> "\n\n")
      flush
  where
    hdrs = [ (hContentType, "text/event-stream")
           , ("Cache-Control", "no-cache")
           , ("Connection", "keep-alive")
           , ("Access-Control-Allow-Origin", "*") ]

-- ── Notebook CRUD ────────────────────────────────────────────────

getNotebookH :: AppState -> Handler Notebook
getNotebookH st = liftIO $ readMVar (stNotebook st)

loadNotebookH :: AppState -> ReactiveNotebook -> LoadRequest -> Handler Notebook
loadNotebookH st rn (LoadRequest path) = liftIO $ do
  let absPath = if "/" `isPrefixOf` path then path else stWorkDir st </> path
  raw <- TIO.readFile absPath
  let segs = parseMarkdown raw
  nid <- readIORef (stNextId st)
  let (cells, nid') = foldl go ([], nid) segs
      nb = Notebook (T.pack path) (reverse cells)
  writeIORef (stNextId st) nid'
  modifyMVar_ (stNotebook st) (\_ -> pure nb)
  rnFireRunAll rn
  pure nb
  where
    go (acc, n) (Prose t)          = (Cell n ProseCell t Nothing Nothing "text/plain" False : acc, n + 1)
    go (acc, n) (CodeBlock _ code) = (Cell n CodeCell code Nothing Nothing "text/plain" False : acc, n + 1)

-- | Save notebook back to markdown file.
saveNotebookH :: AppState -> SaveRequest -> Handler Notebook
saveNotebookH st (SaveRequest mPath) = liftIO $ do
  nb <- readMVar (stNotebook st)
  let path = case mPath of
               Just p  -> p
               Nothing -> T.unpack (nbTitle nb)
      absPath = if "/" `isPrefixOf` path then path else stWorkDir st </> path
      segs = map cellToSegment (nbCells nb)
      pairs = map (\s -> case s of
                     CodeBlock _ _ -> (s, Nothing)  -- don't persist outputs into file
                     _             -> (s, Nothing)) segs
      md = reassemble pairs
  createDirectoryIfMissing True (takeDirectory absPath)
  TIO.writeFile absPath md
  let nb' = nb { nbTitle = T.pack path }
  modifyMVar_ (stNotebook st) (\_ -> pure nb')
  putStrLn $ "[sabela] Saved to: " ++ absPath
  pure nb'
  where
    cellToSegment c = case cellType c of
      ProseCell -> Prose (cellSource c)
      CodeCell  -> CodeBlock "haskell" (cellSource c)

updateCellH :: AppState -> ReactiveNotebook -> Int -> UpdateCell -> Handler Cell
updateCellH st rn cid (UpdateCell src) = liftIO $ do
  rnFireCellEdit rn cid src
  nb <- readMVar (stNotebook st)
  case filter (\c -> cellId c == cid) (nbCells nb) of
    (c:_) -> pure c
    []    -> pure (Cell cid CodeCell src Nothing Nothing "text/plain" True)

insertCellH :: AppState -> InsertCell -> Handler Cell
insertCellH st (InsertCell afterId typ src) = liftIO $ do
  nid <- readIORef (stNextId st)
  writeIORef (stNextId st) (nid + 1)
  let cell = Cell nid typ src Nothing Nothing "text/plain" True
  modifyMVar_ (stNotebook st) $ \nb ->
    pure nb { nbCells = ins afterId cell (nbCells nb) }
  pure cell
  where
    ins (-1) c cs     = c : cs
    ins _    c []     = [c]
    ins aid  c (x:xs)
      | cellId x == aid = x : c : xs
      | otherwise       = x : ins aid c xs

deleteCellH :: AppState -> Int -> Handler Notebook
deleteCellH st cid = liftIO $
  modifyMVar (stNotebook st) $ \nb -> do
    let nb' = nb { nbCells = filter (\c -> cellId c /= cid) (nbCells nb) }
    pure (nb', nb')

runCellH :: ReactiveNotebook -> Int -> Handler RunResult
runCellH rn cid = liftIO $ do
  rnFireRunCell rn cid
  pure (RunResult cid Nothing Nothing "text/plain")

runAllH :: ReactiveNotebook -> Handler RunAllResult
runAllH rn = liftIO $ rnFireRunAll rn >> pure (RunAllResult [])

resetH :: ReactiveNotebook -> AppState -> Handler Notebook
resetH rn st = liftIO $ rnFireReset rn >> readMVar (stNotebook st)

-- ── File explorer ────────────────────────────────────────────────

listFilesH :: AppState -> Maybe Text -> Handler [FileEntry]
listFilesH st mPath = liftIO $ do
  let relPath = maybe "." T.unpack mPath
      absPath = stWorkDir st </> relPath
  canon <- canonicalizePath absPath
  let root = stWorkDir st
  if not (root `isPrefixOfPath` canon) then pure []
  else do
    entries <- listDirectory canon
    fes <- forM (sort entries) $ \name -> do
      let full = canon </> name
      isDir <- doesDirectoryExist full
      pure FileEntry { feName = T.pack name, fePath = T.pack (makeRelative root full), feIsDir = isDir }
    let (dirs, files) = foldr (\e (ds, fs) -> if feIsDir e then (e:ds, fs) else (ds, e:fs)) ([], []) fes
    pure (dirs ++ files)

readFileH :: AppState -> Maybe Text -> Handler Text
readFileH st mPath = liftIO $ do
  let relPath = maybe "" T.unpack mPath
      absPath = stWorkDir st </> relPath
  canon <- canonicalizePath absPath
  if not (stWorkDir st `isPrefixOfPath` canon) then pure "(access denied)"
  else TIO.readFile canon

-- | Create a new file or directory.
createFileH :: AppState -> CreateFileRequest -> Handler FileEntry
createFileH st (CreateFileRequest relPath content isDir) = liftIO $ do
  let absPath = stWorkDir st </> T.unpack relPath
  canon <- canonicalizePath (takeDirectory absPath)
  if not (stWorkDir st `isPrefixOfPath` canon)
    then pure (FileEntry relPath relPath False)
    else do
      if isDir
        then createDirectoryIfMissing True absPath
        else do
          createDirectoryIfMissing True (takeDirectory absPath)
          TIO.writeFile absPath content
      putStrLn $ "[sabela] Created: " ++ absPath
      pure FileEntry { feName = T.pack (last (splitPath' (T.unpack relPath)))
                      , fePath = relPath, feIsDir = isDir }
  where
    splitPath' p = case break (== '/') p of
      (a, [])    -> [a]
      (a, _:bs)  -> a : splitPath' bs

-- | Write content to an existing file.
writeFileH :: AppState -> WriteFileRequest -> Handler Text
writeFileH st (WriteFileRequest relPath content) = liftIO $ do
  let absPath = stWorkDir st </> T.unpack relPath
  canon <- canonicalizePath (takeDirectory absPath)
  if not (stWorkDir st `isPrefixOfPath` canon)
    then pure "access denied"
    else do TIO.writeFile absPath content; pure "ok"

-- ── IDE: Completion & Info ───────────────────────────────────────

completeH :: AppState -> CompleteRequest -> Handler CompleteResult
completeH st (CompleteRequest prefix) = liftIO $ do
  mSess <- readMVar (stSession st)
  case mSess of
    Nothing -> pure (CompleteResult [])
    Just sess -> do
      cs <- queryComplete sess prefix
      pure (CompleteResult cs)

infoH :: AppState -> InfoRequest -> Handler InfoResult
infoH st (InfoRequest name) = liftIO $ do
  mSess <- readMVar (stSession st)
  case mSess of
    Nothing -> pure (InfoResult "No GHCi session")
    Just sess -> do
      -- Try :info first, fall back to :type, then :doc
      info <- queryInfo sess name
      if T.null info || "not in scope" `T.isInfixOf` T.toLower info
        then do
          ty <- queryType sess name
          pure (InfoResult ty)
        else do
          doc <- queryDoc sess name
          if T.null doc || "not found" `T.isInfixOf` T.toLower doc
            then pure (InfoResult info)
            else pure (InfoResult (info <> "\n\n--- Documentation ---\n" <> doc))

-- ── Examples ─────────────────────────────────────────────────────

examplesH :: Handler [Example]
examplesH = pure builtinExamples

builtinExamples :: [Example]
builtinExamples =
  [ Example "Hello World" "Print a greeting"
      "Basics"
      "putStrLn \"Hello, Sabela!\""

  , Example "Fibonacci" "Lazy infinite list"
      "Basics"
      "let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)\nmapM_ print (take 15 fibs)"

  , Example "List comprehension" "Pythagorean triples"
      "Basics"
      "let triples = [(a,b,c) | c <- [1..20], b <- [1..c], a <- [1..b], a*a + b*b == c*c]\nprint triples"

  , Example "Map & Filter" "Higher-order functions"
      "Basics"
      "let xs = [1..20]\nprint $ filter even $ map (^2) xs"

  , Example "Working with Text" "Text manipulation with the text library"
      "Libraries"
      "-- cabal: build-depends: text\nimport qualified Data.Text as T\nimport qualified Data.Text.IO as TIO\n\nlet msg = T.pack \"Hello, World!\"\nTIO.putStrLn $ T.toUpper msg\nTIO.putStrLn $ T.reverse msg\nprint $ T.words msg"

  , Example "HTTP Request" "Fetch a URL with http-conduit"
      "Libraries"
      "-- cabal: build-depends: http-conduit, bytestring\nimport Network.HTTP.Simple\nimport qualified Data.ByteString.Lazy.Char8 as L8\n\nresponse <- httpLBS \"http://httpbin.org/get\"\nL8.putStrLn $ getResponseBody response"

  , Example "JSON Parsing" "Decode JSON with aeson"
      "Libraries"
      "-- cabal: build-depends: aeson, text, bytestring\n{-# LANGUAGE DeriveGeneric #-}\nimport Data.Aeson\nimport GHC.Generics\nimport qualified Data.ByteString.Lazy.Char8 as L8\n\ndata Person = Person { name :: String, age :: Int } deriving (Show, Generic)\ninstance FromJSON Person\n\nlet json = L8.pack \"{\\\"name\\\": \\\"Alice\\\", \\\"age\\\": 30}\"\nprint (decode json :: Maybe Person)"

  , Example "HTML Output" "Render rich HTML output"
      "Display"
      "displayHtml $ unlines\n  [ \"<div style='font-family: sans-serif; padding: 16px;'>\"\n  , \"  <h2 style='color: #4a9eff;'>Hello from Sabela</h2>\"\n  , \"  <p>This is <strong>rich HTML</strong> output.</p>\"\n  , \"  <ul>\"\n  , \"    <li>Item one</li>\"\n  , \"    <li>Item two</li>\"\n  , \"  </ul>\"\n  , \"</div>\"\n  ]"

  , Example "SVG Chart" "Draw an SVG bar chart"
      "Display"
      "let bars = zip [\"Mon\",\"Tue\",\"Wed\",\"Thu\",\"Fri\"] [40,65,30,80,55 :: Int]\n    bar (label, h) i = unlines\n      [ \"<rect x='\" ++ show (i*60+10) ++ \"' y='\" ++ show (100-h) ++ \"' width='40' height='\" ++ show h ++ \"' fill='#89b4fa' rx='4'/>\"\n      , \"<text x='\" ++ show (i*60+30) ++ \"' y='115' text-anchor='middle' fill='#cdd6f4' font-size='11'>\" ++ label ++ \"</text>\" ]\n    svg = \"<svg width='320' height='130' style='background:#1e1e2e;padding:8px'>\" ++ concatMap (\\(b,i) -> bar b i) (zip bars [0..]) ++ \"</svg>\"\ndisplaySvg svg"

  , Example "Markdown Output" "Render formatted markdown"
      "Display"
      "displayMarkdown $ unlines\n  [ \"# Analysis Results\"\n  , \"\"\n  , \"The computation found **42** as the answer.\"\n  , \"\"\n  , \"| Metric | Value |\"\n  , \"|--------|-------|\"\n  , \"| Speed  | Fast  |\"\n  , \"| Memory | Low   |\"\n  ]"

  , Example "Concurrent IO" "Async with threads"
      "Advanced"
      "import Control.Concurrent\nimport Control.Monad\n\nmv <- newMVar (0 :: Int)\nlet inc = modifyMVar_ mv (\\n -> pure (n+1))\nts <- forM [1..100] (\\_ -> forkIO inc)\nmapM_ (\\_ -> threadDelay 1000) ts\nthreadDelay 50000\nresult <- readMVar mv\nputStrLn $ \"Counter: \" ++ show result"

  , Example "QuickCheck" "Property-based testing"
      "Advanced"
      "-- cabal: build-depends: QuickCheck\nimport Test.QuickCheck\n\nlet prop_reverse xs = reverse (reverse xs) == (xs :: [Int])\nquickCheck prop_reverse\n\nlet prop_sort_length xs = length (filter even xs) + length (filter odd xs) == length (xs :: [Int])\nquickCheck prop_sort_length"

  , Example "File I/O" "Read and write files"
      "Advanced"
      "writeFile \"/tmp/sabela-test.txt\" \"Hello from Sabela!\\nLine two.\\n\"\ncontents <- readFile \"/tmp/sabela-test.txt\"\nputStrLn contents\nputStrLn $ \"Lines: \" ++ show (length (lines contents))"
  ]

-- ── Helpers ──────────────────────────────────────────────────────

isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath prefix path =
  let p = addTrailingSlash prefix
  in  p == take (length p) path || prefix == path
  where
    addTrailingSlash s
      | null s        = "/"
      | last s == '/' = s
      | otherwise     = s ++ "/"
