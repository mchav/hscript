module Sabela.Run (
    runScript,
    runScriptCapture,
    resolveDeps,
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Sabela.Parse
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process

runScript :: ScriptFile -> T.Text -> IO ()
runScript sf ghciText = withSystemTempDirectory "sabela" $ \tmpDir -> do
    let ghciPath = tmpDir </> "script.ghci"
        envPath = tmpDir </> ".ghc.environment"
    TIO.writeFile ghciPath ghciText
    mEnv <- resolveIfNeeded envPath (scriptMeta sf)
    runGhc mEnv (scriptMeta sf) ghciPath

runScriptCapture :: ScriptFile -> T.Text -> IO T.Text
runScriptCapture sf ghciText = withSystemTempDirectory "sabela" $ \tmpDir -> do
    let ghciPath = tmpDir </> "script.ghci"
        envPath = tmpDir </> ".ghc.environment"
    TIO.writeFile ghciPath ghciText
    mEnv <- resolveIfNeeded envPath (scriptMeta sf)
    captureGhc mEnv (scriptMeta sf) ghciPath

resolveIfNeeded :: FilePath -> CabalMeta -> IO (Maybe FilePath)
resolveIfNeeded envPath CabalMeta{..}
    | null metaDeps = pure Nothing
    | otherwise = do
        resolveDeps envPath metaDeps
        pure (Just envPath)

resolveDeps :: FilePath -> [T.Text] -> IO ()
resolveDeps envPath deps = do
    let args =
            ["-v0", "install", "--lib", "--package-env=" ++ envPath]
                ++ map T.unpack deps
        cp = (proc "cabal" args){delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> do
            putStrLn $ "sabela: cabal install --lib failed (exit " ++ show n ++ ")"
            exitWith code

ghcArgs :: Maybe FilePath -> CabalMeta -> FilePath -> [String]
ghcArgs mEnv CabalMeta{..} ghciPath =
    let envFlags = maybe [] (\env -> ["-package-env=" ++ env]) mEnv
        extFlags = map (\e -> "-X" ++ T.unpack e) metaExts
        optFlags = map T.unpack metaGhcOptions
        scriptArg = ":script " ++ ghciPath
     in envFlags ++ extFlags ++ optFlags ++ ["-e", scriptArg]

runGhc :: Maybe FilePath -> CabalMeta -> FilePath -> IO ()
runGhc mEnv meta ghciPath = do
    let args = ghcArgs mEnv meta ghciPath
        cp = (proc "ghc" args){delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> exitWith code

captureGhc :: Maybe FilePath -> CabalMeta -> FilePath -> IO T.Text
captureGhc mEnv meta ghciPath = do
    let args = ghcArgs mEnv meta ghciPath
    (code, out, err) <- readProcessWithExitCode "ghc" args ""
    case code of
        ExitSuccess -> pure (T.pack out)
        ExitFailure _ -> do
            putStrLn err
            pure (T.pack out)
