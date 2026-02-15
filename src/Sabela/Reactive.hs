module Sabela.Reactive
  ( ReactiveNotebook(..)
  , setupReactive
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (readMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when, void)
import Data.Char (isDigit, isAlpha, isAlphaNum)
import Data.IORef
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import Reactive.Banana
import Reactive.Banana.Frameworks

import Sabela.Parse (ScriptFile(..), CabalMeta(..), parseScript)
import Sabela.Resolve (resolveRemotes)
import Sabela.Transform (toGhciScript)
import Sabela.Run (resolveDeps)
import Sabela.Session
import Sabela.Server.Types

-- ═══════════════════════════════════════════════════════════════════
-- Reactive notebook handle
-- ═══════════════════════════════════════════════════════════════════

data ReactiveNotebook = ReactiveNotebook
  { rnFireCellEdit :: Int -> Text -> IO ()
  , rnFireRunCell  :: Int -> IO ()
  , rnFireRunAll   :: IO ()
  , rnFireReset    :: IO ()
  }

setupReactive :: AppState -> IO ReactiveNotebook
setupReactive st = do
  (editAddH,    fireEdit)    <- newAddHandler
  (runCellAddH, fireRunCell) <- newAddHandler
  (runAllAddH,  fireRunAll)  <- newAddHandler
  (resetAddH,   fireReset)   <- newAddHandler

  network <- compile $ do
    eEdit    <- fromAddHandler editAddH
    eRunCell <- fromAddHandler runCellAddH
    eRunAll  <- fromAddHandler runAllAddH
    eReset   <- fromAddHandler resetAddH

    reactimate $ fmap (\(cid, src) -> handleCellEdit st cid src) eEdit
    reactimate $ fmap (\cid        -> handleRunCell st cid)      eRunCell
    reactimate $ fmap (\()         -> handleRunAll st)           eRunAll
    reactimate $ fmap (\()         -> handleReset st)            eReset

  actuate network
  pure ReactiveNotebook
    { rnFireCellEdit = \cid src -> fireEdit (cid, src)
    , rnFireRunCell  = fireRunCell
    , rnFireRunAll   = fireRunAll ()
    , rnFireReset    = fireReset ()
    }

-- ═══════════════════════════════════════════════════════════════════
-- MIME protocol
-- ═══════════════════════════════════════════════════════════════════

mimeMarkerPrefix :: Text
mimeMarkerPrefix = "---MIME:"

mimeMarkerSuffix :: Text
mimeMarkerSuffix = "---"

parseMimeOutput :: Text -> (Text, Text)
parseMimeOutput raw =
  case T.lines raw of
    (firstLine : rest)
      | Just afterPrefix <- T.stripPrefix mimeMarkerPrefix firstLine
      , Just mimeType    <- T.stripSuffix mimeMarkerSuffix afterPrefix
      , not (T.null mimeType)
      -> (T.strip mimeType, T.unlines rest)
    _ -> ("text/plain", raw)

displayPrelude :: Text
displayPrelude = T.unlines
  [ ":{"
  , "let { displayMime_ t c = putStrLn (\"---MIME:\" ++ t ++ \"---\") >> putStrLn c"
  , "    ; displayHtml     = displayMime_ \"text/html\""
  , "    ; displayMarkdown = displayMime_ \"text/markdown\""
  , "    ; displaySvg      = displayMime_ \"image/svg+xml\""
  , "    ; displayLatex    = displayMime_ \"text/latex\""
  , "    ; displayJson     = displayMime_ \"application/json\""
  , "    ; displayImage mime b64 = putStrLn (\"---MIME:\" ++ mime ++ \";base64---\") >> putStrLn b64"
  , "    }"
  , ":}"
  ]

-- ═══════════════════════════════════════════════════════════════════
-- Cell dependency analysis
--
-- For each code cell we extract:
--   DEFINES: names bound at top-level  (x = ..., let x = ..., x <- ...)
--   USES:    all identifier tokens      (over-approximation; safe)
--
-- On edit of cell N:
--   1. Start from cell N onward (earlier cells unaffected)
--   2. changed_names = ∅
--   3. For each cell from N..end:
--        if cell was edited  OR  uses(cell) ∩ changed_names ≠ ∅
--        then RE-RUN this cell, changed_names ∪= defines(cell)
--        else SKIP this cell
--
-- This handles transitive deps:
--   Cell A defines x → Cell B uses x and defines y → Cell C uses y
--   Editing A: B runs (x changed), adds y → C runs (y changed)
--   Cell D uses only z → skipped entirely.
-- ═══════════════════════════════════════════════════════════════════

-- | (names defined, names used) by a cell.
cellNames :: Text -> (Set.Set Text, Set.Set Text)
cellNames src = (defs, uses)
  where
    ls   = T.lines src
    defs = Set.fromList $ concatMap extractDefs ls
    uses = Set.fromList $ concatMap extractTokens ls

-- | Extract top-level names defined by a single source line.
-- Handles: x = ..., let x = ..., x <- ..., data X, type X, etc.
extractDefs :: Text -> [Text]
extractDefs line
  | T.null s                           = []
  | T.isPrefixOf "--" s                = []   -- comment
  | T.isPrefixOf ":" s                 = []   -- GHCi command
  | T.isPrefixOf "import " s           = []
  | T.isPrefixOf "{-#" s              = []   -- pragma
  -- let binding: "let x = ..."
  | Just rest <- stripKW "let" s       = firstLowerIdent rest
  -- type-level: "data X", "type X", "newtype X", "class X"
  | Just rest <- stripKW "data" s      = firstAnyIdent rest
  | Just rest <- stripKW "type" s      = firstAnyIdent rest
  | Just rest <- stripKW "newtype" s   = firstAnyIdent rest
  | Just rest <- stripKW "class" s     = firstAnyIdent rest
  -- value binding: "name ... =" or monadic bind: "name <- ..."
  | otherwise =
      let toks = T.words s
      in case toks of
           (w : rest)
             | isLowerIdent w
             , any (\t -> t == "=" || t == "<-") (take 8 rest)
             -> [w]
           _ -> []
  where
    s = T.strip line

stripKW :: Text -> Text -> Maybe Text
stripKW kw t = case T.stripPrefix kw t of
  Just rest
    | T.null rest                     -> Nothing
    | not (isIdentChar (T.head rest)) -> Just (T.stripStart rest)
  _ -> Nothing

firstLowerIdent :: Text -> [Text]
firstLowerIdent t =
  let w = T.takeWhile isIdentChar (T.stripStart t)
  in  [w | isLowerIdent w]

firstAnyIdent :: Text -> [Text]
firstAnyIdent t =
  let w = T.takeWhile isIdentChar (T.stripStart t)
  in  [w | not (T.null w), isAlpha (T.head w) || T.head w == '_']

isLowerIdent :: Text -> Bool
isLowerIdent t = not (T.null t) &&
  let c = T.head t in (isAlpha c && not (c >= 'A' && c <= 'Z')) || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Extract all identifier tokens from a line.
extractTokens :: Text -> [Text]
extractTokens = filter isIdent . T.split (not . isIdentChar)
  where
    isIdent t = not (T.null t) && (isAlpha (T.head t) || T.head t == '_')

-- | Given edited cell IDs, select which cells actually need re-execution
-- via dependency propagation. Only cells from the earliest edited cell
-- onward are passed in.
selectAffected :: Set.Set Int -> [Cell] -> [Cell]
selectAffected editedSet = go Set.empty
  where
    go _       []     = []
    go changed (c:cs) =
      let (defs, uses) = cellNames (cellSource c)
          isEdited     = Set.member (cellId c) editedSet
          isAffected   = not (Set.null (Set.intersection uses changed))
      in  if isEdited || isAffected
            then c : go (Set.union changed defs) cs
            else     go changed cs

-- ═══════════════════════════════════════════════════════════════════
-- Structured error parsing
-- ═══════════════════════════════════════════════════════════════════

parseErrors :: Text -> [CellError]
parseErrors stderr
  | T.null stderr = []
  | otherwise     = concatMap parseSingleError (splitErrors stderr)
  where
    parseSingleError block =
      let ls = T.lines block
      in case ls of
           (hdr : _) -> case parseErrorHeader hdr of
             Just (ln, col) -> [CellError (Just ln) col (T.strip block)]
             Nothing        -> [CellError Nothing Nothing (T.strip block)]
           _ -> []

    splitErrors t = filter (not . T.null . T.strip) $ splitOnHeaders (T.lines t) [] []

    splitOnHeaders [] current acc =
      let b = T.unlines (reverse current)
      in  reverse (if T.null (T.strip b) then acc else b : acc)
    splitOnHeaders (l:ls) current acc
      | isErrorHeader l && not (null current) =
          let b = T.unlines (reverse current)
          in  splitOnHeaders ls [l] (b : acc)
      | otherwise = splitOnHeaders ls (l : current) acc

    isErrorHeader l = "<interactive>:" `T.isPrefixOf` l
                   || "<cell>:" `T.isPrefixOf` l

parseErrorHeader :: Text -> Maybe (Int, Maybe Int)
parseErrorHeader hdr =
  let after = case T.stripPrefix "<interactive>:" hdr of
                Just r  -> Just r
                Nothing -> T.stripPrefix "<cell>:" hdr
  in case after of
    Nothing -> Nothing
    Just rest ->
      let (lineStr, rest2) = T.span isDigit rest
      in case reads (T.unpack lineStr) :: [(Int, String)] of
           [(ln, _)] ->
             let col = case T.stripPrefix ":" rest2 of
                   Just r3 -> case reads (T.unpack (T.takeWhile isDigit r3)) :: [(Int, String)] of
                                [(c, _)] -> Just c
                                _        -> Nothing
                   Nothing -> Nothing
             in  Just (ln, col)
           _ -> Nothing

-- ═══════════════════════════════════════════════════════════════════
-- Event handlers
-- ═══════════════════════════════════════════════════════════════════

-- | Cell edited → save source, debounce, smart re-execution.
handleCellEdit :: AppState -> Int -> Text -> IO ()
handleCellEdit st cid src = do
  putStrLn $ "[reactive] handleCellEdit: cell " ++ show cid
  modifyMVar_ (stNotebook st) $ \nb ->
    pure nb { nbCells = map upd (nbCells nb) }
  debounceExecuteAffected st cid
  where
    upd c | cellId c == cid = c { cellSource = src, cellDirty = True }
          | otherwise       = c

-- | Debounce with accumulation of edited cell IDs.
-- Multiple rapid edits within 500ms merge — we collect the union
-- of all edited cell IDs, then run the dependency-aware selection.
debounceExecuteAffected :: AppState -> Int -> IO ()
debounceExecuteAffected st cid = do
  gen <- bumpGeneration st
  modifyMVar_ (stDebounceRef st) $ \mOld -> pure $ Just $ case mOld of
    Just (_, oldCids) -> (gen, Set.insert cid oldCids)
    Nothing           -> (gen, Set.singleton cid)
  void $ forkIO $ do
    threadDelay 500000   -- 500ms debounce
    mCurrent <- readMVar (stDebounceRef st)
    case mCurrent of
      Just (g, editedCids) | g == gen -> do
        modifyMVar_ (stDebounceRef st) (\_ -> pure Nothing)
        smartExecuteAffected st gen editedCids
      _ -> pure ()   -- superseded by newer edit/action

-- | Explicit run-one-cell button.
handleRunCell :: AppState -> Int -> IO ()
handleRunCell st cid = do
  putStrLn $ "[reactive] handleRunCell: cell " ++ show cid
  gen <- bumpGeneration st
  modifyMVar_ (stDebounceRef st) (\_ -> pure Nothing)
  void $ forkIO $ executeSingleCell st gen cid

-- | Explicit run-all button → full restart.
handleRunAll :: AppState -> IO ()
handleRunAll st = do
  putStrLn "[reactive] handleRunAll: full restart"
  gen <- bumpGeneration st
  modifyMVar_ (stDebounceRef st) (\_ -> pure Nothing)
  void $ forkIO $ executeFullRestart st gen

handleReset :: AppState -> IO ()
handleReset st = do
  putStrLn "[reactive] handleReset"
  void $ bumpGeneration st
  modifyMVar_ (stDebounceRef st) (\_ -> pure Nothing)
  killSession st
  modifyMVar_ (stNotebook st) $ \nb ->
    pure nb { nbCells = map clr (nbCells nb) }
  broadcast st (EvSessionStatus "reset")
  where
    clr c = c { cellOutput = Nothing, cellError = Nothing
              , cellMime = "text/plain", cellDirty = False }

-- ═══════════════════════════════════════════════════════════════════
-- Execution modes
-- ═══════════════════════════════════════════════════════════════════

-- | After debounce: dependency-aware execution.
-- Only re-runs edited cells + cells that transitively depend on them.
smartExecuteAffected :: AppState -> Int -> Set.Set Int -> IO ()
smartExecuteAffected st gen editedCids = do
  putStrLn $ "[reactive] smartExecuteAffected: editedCids="
          ++ show (Set.toList editedCids)
  nb <- readMVar (stNotebook st)
  let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
      needed  = collectMeta [cellSource c | c <- allCode]

  installed <- readIORef (stInstalledDeps st)
  instExts  <- readIORef (stInstalledExts st)
  mSess     <- readMVar  (stSession st)

  let neededD = Set.fromList (nmDeps needed)
      neededE = Set.fromList (nmExts needed)
      depsOk  = neededD == installed
      extsOk  = neededE == instExts

  case (mSess, depsOk && extsOk) of
    -- ── Happy path: session alive, deps unchanged ──
    (Just _, True) -> do
      -- Trim to cells from the earliest edited cell onward
      let fromEdited = dropWhile (\c -> not (Set.member (cellId c) editedCids)) allCode
          toRun      = selectAffected editedCids fromEdited
          allIds     = map cellId allCode
          fromIds    = map cellId fromEdited
          runIds     = map cellId toRun
          skipIds    = filter (`notElem` runIds) fromIds

      -- ── Diagnostic logging ──
      putStrLn $ "[reactive] All code cells: " ++ show allIds
      putStrLn $ "[reactive] From edited cell onward: " ++ show fromIds
      putStrLn $ "[reactive] WILL RUN (affected): " ++ show runIds
      putStrLn $ "[reactive] WILL SKIP (unaffected): " ++ show skipIds

      -- Log per-cell dependency info
      forM_ fromEdited $ \c -> do
        let (defs, uses) = cellNames (cellSource c)
        putStrLn $ "[reactive]   cell " ++ show (cellId c)
               ++ " defines=" ++ show (Set.toList defs)
               ++ " uses=" ++ show (take 10 (Set.toList uses))
               ++ (if Set.size uses > 10 then "..." else "")

      runCellList st gen toRun

    -- ── Deps changed → full restart ──
    (_, False) -> do
      putStrLn "[reactive] Deps/exts changed → full restart"
      ok <- installAndRestart st gen needed
      when ok $ runCellList st gen allCode

    -- ── No session → create + run all ──
    (Nothing, True) -> do
      putStrLn "[reactive] No session → starting fresh, running all"
      ok <- installAndRestart st gen needed
      when ok $ runCellList st gen allCode

-- | Run a single cell in the existing session.
executeSingleCell :: AppState -> Int -> Int -> IO ()
executeSingleCell st gen cid = do
  putStrLn $ "[reactive] executeSingleCell: cell " ++ show cid
  nb <- readMVar (stNotebook st)
  let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
      needed  = collectMeta [cellSource c | c <- allCode]

  ok <- ensureSessionAlive st gen needed
  when ok $
    case filter (\c -> cellId c == cid) allCode of
      (cell:_) -> do
        still <- isCurrentGen st gen
        when still $ runAndBroadcast st gen cell
        still' <- isCurrentGen st gen
        when still' $ broadcast st EvExecutionDone
      [] -> broadcast st EvExecutionDone

-- | Full restart: kill session, reinstall, run all.
executeFullRestart :: AppState -> Int -> IO ()
executeFullRestart st gen = do
  putStrLn "[reactive] executeFullRestart: killing session, running all"
  nb <- readMVar (stNotebook st)
  let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
      needed  = collectMeta [cellSource c | c <- allCode]

  killSession st
  ok <- installAndRestart st gen needed
  when ok $ runCellList st gen allCode

-- | Run a list of cells sequentially.
runCellList :: AppState -> Int -> [Cell] -> IO ()
runCellList st gen cells = do
  forM_ cells $ \cell -> do
    still <- isCurrentGen st gen
    when still $ runAndBroadcast st gen cell
  still <- isCurrentGen st gen
  when still $ broadcast st EvExecutionDone

runAndBroadcast :: AppState -> Int -> Cell -> IO ()
runAndBroadcast st gen cell = do
  broadcast st (EvCellUpdating (cellId cell))
  (result, errs) <- execCell st cell
  still <- isCurrentGen st gen
  when still $ do
    modifyMVar_ (stNotebook st) $ \nb ->
      pure nb { nbCells = map (applyResult result) (nbCells nb) }
    broadcast st (EvCellResult (rrCellId result)
                                (rrOutput result)
                                (rrError result)
                                (rrMime result)
                                errs)

-- ═══════════════════════════════════════════════════════════════════
-- Session management
-- ═══════════════════════════════════════════════════════════════════

data NeededMeta = NeededMeta { nmDeps :: [Text], nmExts :: [Text] }

collectMeta :: [Text] -> NeededMeta
collectMeta sources =
  let ms = map extractMeta sources
  in  NeededMeta (nub $ concatMap metaDeps ms)
                 (nub $ concatMap metaExts ms)

extractMeta :: Text -> CabalMeta
extractMeta src = case parseScript "<meta>" src of
  Right sf -> scriptMeta sf
  Left _   -> CabalMeta [] [] []

ensureSessionAlive :: AppState -> Int -> NeededMeta -> IO Bool
ensureSessionAlive st gen needed = do
  installed <- readIORef (stInstalledDeps st)
  instExts  <- readIORef (stInstalledExts st)
  mSess     <- readMVar  (stSession st)
  let ok = Set.fromList (nmDeps needed) == installed
        && Set.fromList (nmExts needed) == instExts
  case mSess of
    Just _ | ok -> pure True
    _           -> installAndRestart st gen needed

installAndRestart :: AppState -> Int -> NeededMeta -> IO Bool
installAndRestart st gen NeededMeta{..} = do
  still <- isCurrentGen st gen
  if not still then pure False else do
    installed <- readIORef (stInstalledDeps st)
    let neededD = Set.fromList nmDeps
        depsOk  = neededD == installed

    when (not depsOk) $ do
      let brand_new = Set.difference neededD installed
      broadcast st $ EvSessionStatus $
        if Set.null brand_new then "updating dependencies"
        else "installing: " <> T.intercalate ", " (Set.toList brand_new)
      if null nmDeps
        then do
          writeIORef (stEnvFile st) Nothing
          writeIORef (stInstalledDeps st) Set.empty
        else do
          let envPath = stTmpDir st </> ".ghc.environment"
          putStrLn $ "[sabela] cabal install --lib " ++ unwords (map T.unpack nmDeps)
          resolveDeps envPath nmDeps
          writeIORef (stEnvFile st) (Just envPath)
          writeIORef (stInstalledDeps st) neededD

    writeIORef (stInstalledExts st) (Set.fromList nmExts)
    broadcast st (EvSessionStatus "starting session")
    killSession st
    startSessionWith st nmDeps nmExts
    pure True

startSessionWith :: AppState -> [Text] -> [Text] -> IO ()
startSessionWith st deps exts = do
  envFile <- readIORef (stEnvFile st)
  let cfg = SessionConfig
        { scDeps = deps, scExts = exts, scGhcOptions = [], scEnvFile = envFile }
  putStrLn $ "[sabela] Starting GHCi (deps=" ++ show deps ++ ")"
  sess <- newSession cfg
  putStrLn "[sabela] Injecting display prelude"
  _ <- runBlock sess displayPrelude
  modifyMVar_ (stSession st) (\_ -> pure (Just sess))
  broadcast st (EvSessionStatus "ready")

killSession :: AppState -> IO ()
killSession st =
  modifyMVar_ (stSession st) $ \mSess -> do
    case mSess of
      Just s  -> void (try (closeSession s) :: IO (Either SomeException ()))
      Nothing -> pure ()
    pure Nothing

-- ═══════════════════════════════════════════════════════════════════
-- Cell execution
-- ═══════════════════════════════════════════════════════════════════

execCell :: AppState -> Cell -> IO (RunResult, [CellError])
execCell st cell = do
  mSess <- readMVar (stSession st)
  case mSess of
    Nothing -> pure (RunResult (cellId cell) Nothing (Just "No GHCi session") "text/plain", [])
    Just sess ->
      case parseScript "<cell>" (cellSource cell) of
        Left err -> pure (RunResult (cellId cell) Nothing (Just (T.pack err)) "text/plain",
                          [CellError Nothing Nothing (T.pack err)])
        Right sf -> do
          resolved <- resolveRemotes (scriptLines sf)
          let ghci = toGhciScript resolved
          putStrLn $ "[sabela] Cell " ++ show (cellId cell) ++ ":\n" ++ T.unpack ghci
          (rawOut, rawErr) <- runBlock sess ghci
          let (mime, body) = parseMimeOutput rawOut
              errs = parseErrors rawErr
          putStrLn $ "[sabela]  → mime=" ++ T.unpack mime
                   ++ " out=" ++ show (T.take 80 body)
                   ++ " errors=" ++ show (length errs)
          pure ( RunResult
                   { rrCellId = cellId cell
                   , rrOutput = if T.null body then Nothing else Just body
                   , rrError  = if T.null rawErr then Nothing else Just rawErr
                   , rrMime   = mime
                   }
               , errs )

-- ═══════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════

bumpGeneration :: AppState -> IO Int
bumpGeneration st =
  atomicModifyIORef' (stGeneration st) (\g -> let g' = g + 1 in (g', g'))

isCurrentGen :: AppState -> Int -> IO Bool
isCurrentGen st gen = (== gen) <$> readIORef (stGeneration st)

broadcast :: AppState -> NotebookEvent -> IO ()
broadcast st ev = atomically $ writeTChan (stBroadcast st) ev

applyResult :: RunResult -> Cell -> Cell
applyResult r c
  | cellId c == rrCellId r = c { cellOutput = rrOutput r
                                , cellError  = rrError r
                                , cellMime   = rrMime r
                                , cellDirty  = False }
  | otherwise = c
