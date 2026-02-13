module HScript.Run
  ( runScript
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..), exitWith)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Process (proc, waitForProcess, createProcess, CreateProcess(..))
import HScript.Parse

runScript :: ScriptFile -> T.Text -> IO ()
runScript ScriptFile{..} ghciText = withSystemTempDirectory "hscript" $ \tmpDir -> do
  let ghciPath = tmpDir </> "script.ghci"
      envPath  = tmpDir </> ".ghc.environment"
  TIO.writeFile ghciPath ghciText

  let CabalMeta{..} = scriptMeta
  if null metaDeps
    then runGhc Nothing metaExts metaGhcOptions ghciPath
    else do
      resolveDeps envPath metaDeps
      runGhc (Just envPath) metaExts metaGhcOptions ghciPath

resolveDeps :: FilePath -> [T.Text] -> IO ()
resolveDeps envPath deps = do
  let args = ["-v0", "install", "--lib", "--package-env=" ++ envPath]
             ++ map T.unpack deps
      cp = (proc "cabal" args)
           { delegate_ctlc = True }
  (_, _, _, ph) <- createProcess cp
  code <- waitForProcess ph
  case code of
    ExitSuccess   -> pure ()
    ExitFailure n -> do
      putStrLn $ "hscript: cabal install --lib failed (exit " ++ show n ++ ")"
      exitWith code

runGhc :: Maybe FilePath -> [T.Text] -> [T.Text] -> FilePath -> IO ()
runGhc mEnv exts ghcOpts ghciPath = do
  let envFlags = case mEnv of
        Nothing  -> []
        Just env -> ["-package-env=" ++ env]
      extFlags = map (\e -> "-X" ++ T.unpack e) exts
      optFlags = map T.unpack ghcOpts
      scriptArg = ":script " ++ ghciPath
      args = envFlags ++ extFlags ++ optFlags ++ ["-e", scriptArg]
      cp = (proc "ghc" args)
           { delegate_ctlc = True }
  (_, _, _, ph) <- createProcess cp
  code <- waitForProcess ph
  case code of
    ExitSuccess   -> pure ()
    ExitFailure _ -> exitWith code
