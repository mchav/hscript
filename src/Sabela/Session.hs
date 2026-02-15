module Sabela.Session
  ( Session
  , SessionConfig(..)
  , sessConfig
  , newSession
  , closeSession
  , runBlock
  , resetSession
  , queryComplete
  , queryType
  , queryInfo
  , queryDoc
  ) where

import Control.Concurrent (MVar, newMVar, withMVar, forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, try)
import Control.Monad (when, forever)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (Handle, hSetBuffering, hSetEncoding, BufferMode(..), hFlush,
                  hClose, hIsEOF, utf8, hGetLine, hPutStrLn)
import System.Process (CreateProcess(..), StdStream(..), ProcessHandle,
                      createProcess, proc, waitForProcess)

-- | A running GHCi session.
data Session = Session
  { sessLock    :: MVar ()
  , sessStdin   :: Handle
  , sessStdout  :: Handle
  , sessStderr  :: Handle
  , sessProc    :: ProcessHandle
  , sessLines   :: Chan Text
  , sessErrBuf  :: IORef [Text]
  , sessCounter :: IORef Int
  , sessConfig  :: SessionConfig
  }

data SessionConfig = SessionConfig
  { scDeps       :: [Text]
  , scExts       :: [Text]
  , scGhcOptions :: [Text]
  , scEnvFile    :: Maybe FilePath
  } deriving (Show)

-- | Start a new GHCi session.
newSession :: SessionConfig -> IO Session
newSession cfg = do
  let envFlags = case scEnvFile cfg of
        Nothing  -> []
        Just env -> ["-package-env=" ++ env]
      extFlags = map (\e -> "-X" ++ T.unpack e) (scExts cfg)
      optFlags = map T.unpack (scGhcOptions cfg)
      args = ["--interactive", "-ignore-dot-ghci", "-v0"]
             ++ envFlags ++ extFlags ++ optFlags
      cp = (proc "ghc" args)
           { std_in  = CreatePipe
           , std_out = CreatePipe
           , std_err = CreatePipe
           , delegate_ctlc = False
           }

  (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp

  hSetBuffering hIn  LineBuffering
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  hSetEncoding  hIn  utf8
  hSetEncoding  hOut utf8
  hSetEncoding  hErr utf8

  lock    <- newMVar ()
  lineCh  <- newChan
  errBuf  <- newIORef []
  counter <- newIORef 0

  _ <- forkIO $ readLoop hOut lineCh
  _ <- forkIO $ errLoop hErr errBuf

  let sess = Session lock hIn hOut hErr ph lineCh errBuf counter cfg

  sendRaw sess ":set prompt \"\""
  sendRaw sess ":set prompt-cont \"\""

  mk <- freshMarker sess
  sendRaw sess $ "putStrLn " ++ show (T.unpack mk)
  _ <- drainUntilMarker sess mk

  pure sess

closeSession :: Session -> IO ()
closeSession Session{..} = do
  _ <- try (hPutStrLn sessStdin ":quit") :: IO (Either SomeException ())
  _ <- try (hFlush sessStdin)             :: IO (Either SomeException ())
  _ <- try (hClose sessStdin)             :: IO (Either SomeException ())
  _ <- waitForProcess sessProc
  pure ()

-- | Run a block of GHCi commands, returning (stdout, stderr).
runBlock :: Session -> Text -> IO (Text, Text)
runBlock sess block = withMVar (sessLock sess) $ \_ -> do
  atomicModifyIORef' (sessErrBuf sess) (\_ -> ([], ()))
  mk <- freshMarker sess
  mapM_ (sendRaw sess . T.unpack) (T.lines block)
  sendRaw sess $ "putStrLn " ++ show (T.unpack mk)
  outLines <- drainUntilMarker sess mk
  errLines <- readIORef (sessErrBuf sess)
  pure (T.strip $ T.unlines outLines,
        T.strip $ T.unlines (reverse errLines))

resetSession :: Session -> IO Session
resetSession sess = do
  closeSession sess
  newSession (sessConfig sess)

-- ── IDE query commands ───────────────────────────────────────────

-- | Tab-completion via :complete repl "prefix".
-- Returns list of completions.
queryComplete :: Session -> Text -> IO [Text]
queryComplete sess prefix = withMVar (sessLock sess) $ \_ -> do
  atomicModifyIORef' (sessErrBuf sess) (\_ -> ([], ()))
  mk <- freshMarker sess
  let cmd = ":complete repl \"" ++ T.unpack (escapeGhci prefix) ++ "\""
  sendRaw sess cmd
  sendRaw sess $ "putStrLn " ++ show (T.unpack mk)
  outLines <- drainUntilMarker sess mk
  -- GHCi :complete output format:
  --   N M total
  --   "completion1"
  --   "completion2"
  --   ...
  pure $ concatMap parseCompletionLine outLines

-- | :type expression
queryType :: Session -> Text -> IO Text
queryType sess expr = withMVar (sessLock sess) $ \_ -> do
  atomicModifyIORef' (sessErrBuf sess) (\_ -> ([], ()))
  mk <- freshMarker sess
  sendRaw sess $ ":type " ++ T.unpack expr
  sendRaw sess $ "putStrLn " ++ show (T.unpack mk)
  outLines <- drainUntilMarker sess mk
  errLines <- readIORef (sessErrBuf sess)
  let out = T.strip $ T.unlines outLines
  if T.null out
    then pure $ T.strip $ T.unlines (reverse errLines)
    else pure out

-- | :info name
queryInfo :: Session -> Text -> IO Text
queryInfo sess name = withMVar (sessLock sess) $ \_ -> do
  atomicModifyIORef' (sessErrBuf sess) (\_ -> ([], ()))
  mk <- freshMarker sess
  sendRaw sess $ ":info " ++ T.unpack name
  sendRaw sess $ "putStrLn " ++ show (T.unpack mk)
  outLines <- drainUntilMarker sess mk
  errLines <- readIORef (sessErrBuf sess)
  let out = T.strip $ T.unlines outLines
  if T.null out
    then pure $ T.strip $ T.unlines (reverse errLines)
    else pure out

-- | :doc name (GHC 9.4+, may not be available)
queryDoc :: Session -> Text -> IO Text
queryDoc sess name = withMVar (sessLock sess) $ \_ -> do
  atomicModifyIORef' (sessErrBuf sess) (\_ -> ([], ()))
  mk <- freshMarker sess
  sendRaw sess $ ":doc " ++ T.unpack name
  sendRaw sess $ "putStrLn " ++ show (T.unpack mk)
  outLines <- drainUntilMarker sess mk
  errLines <- readIORef (sessErrBuf sess)
  let out = T.strip $ T.unlines outLines
  if T.null out
    then pure $ T.strip $ T.unlines (reverse errLines)
    else pure out

-- ── Internal helpers ─────────────────────────────────────────────

freshMarker :: Session -> IO Text
freshMarker Session{..} = do
  n <- atomicModifyIORef' sessCounter (\i -> (i + 1, i))
  pure $ "---SABELA_MARKER_" <> T.pack (show n) <> "---"

sendRaw :: Session -> String -> IO ()
sendRaw Session{..} cmd = do
  hPutStrLn sessStdin cmd
  hFlush sessStdin

readLoop :: Handle -> Chan Text -> IO ()
readLoop h ch = do
  _ <- try (forever go) :: IO (Either SomeException ())
  pure ()
  where
    go = do
      eof <- hIsEOF h
      if eof then writeChan ch "---EOF---"
             else do line <- hGetLine h; writeChan ch (T.pack line)

errLoop :: Handle -> IORef [Text] -> IO ()
errLoop h ref = do
  _ <- try (forever go) :: IO (Either SomeException ())
  pure ()
  where
    go = do
      eof <- hIsEOF h
      when (not eof) $ do
        line <- hGetLine h
        atomicModifyIORef' ref (\ls -> (T.pack line : ls, ()))

drainUntilMarker :: Session -> Text -> IO [Text]
drainUntilMarker Session{..} mk = go []
  where
    go acc = do
      line <- readChan sessLines
      if line == mk then pure (reverse acc)
      else if line == "---EOF---" then pure (reverse acc)
      else go (line : acc)

-- | Escape a string for use inside GHCi double quotes.
escapeGhci :: Text -> Text
escapeGhci = T.concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c    = T.singleton c

-- | Parse a single :complete output line (strips quotes).
parseCompletionLine :: Text -> [Text]
parseCompletionLine line =
  let stripped = T.strip line
  in  case T.stripPrefix "\"" stripped of
        Just rest -> case T.stripSuffix "\"" rest of
          Just inner -> [inner]
          Nothing    -> []
        Nothing -> []  -- skip the "N M total" header line
