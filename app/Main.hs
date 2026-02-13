module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO

import HScript.Parse (parseScript, ScriptFile(..))
import HScript.Resolve (resolveRemotes)
import HScript.Transform (toGhciScript)
import HScript.Run (runScript)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--debug", path] -> debug path
    [path]            -> run path
    _                 -> do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " [--debug] <script.hs>"
      exitFailure

run :: FilePath -> IO ()
run path = do
  input <- TIO.readFile path
  case parseScript path input of
    Left err -> do
      putStrLn $ "hscript: parse error:\n" ++ err
      exitFailure
    Right sf -> do
      resolved <- resolveRemotes (scriptLines sf)
      let ghci = toGhciScript resolved
      runScript (sf { scriptLines = resolved }) ghci

debug :: FilePath -> IO ()
debug path = do
  input <- TIO.readFile path
  case parseScript path input of
    Left err -> do
      putStrLn $ "hscript: parse error:\n" ++ err
      exitFailure
    Right sf -> do
      resolved <- resolveRemotes (scriptLines sf)
      let ghci = toGhciScript resolved
      TIO.putStrLn ghci
