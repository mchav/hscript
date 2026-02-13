module HScript.Resolve
  ( resolveRemotes
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..), exitFailure)
import System.Process (readProcessWithExitCode)

import HScript.Parse (Line(..), ScriptFile(..), parseScript)

-- Recursively resolves all nested :remoteScript directives.
resolveRemotes :: [Line] -> IO [Line]
resolveRemotes = fmap concat . mapM resolve

resolve :: Line -> IO [Line]
resolve (RemoteScript url) = do
  content <- fetchUrl url
  case parseScript (T.unpack url) content of
    Left err -> do
      putStrLn $ "hscript: error parsing remote script " ++ T.unpack url ++ ":\n" ++ err
      exitFailure
    Right sf -> resolveRemotes (scriptLines sf)
resolve l = pure [l]

fetchUrl :: Text -> IO Text
fetchUrl url = do
  (code, out, err) <- readProcessWithExitCode
    "curl" ["-sfL", T.unpack url] ""
  case code of
    ExitSuccess   -> pure (T.pack out)
    ExitFailure n -> do
      putStrLn $ "hscript: failed to fetch " ++ T.unpack url
                 ++ " (exit " ++ show n ++ ")"
      putStrLn err
      exitFailure
