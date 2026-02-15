module Main (main) where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)

import Sabela.Notebook (debugNotebook, runNotebook)
import Sabela.Parse (ScriptFile (..), parseScript)
import Sabela.Resolve (resolveRemotes)
import Sabela.Run (runScript)
import Sabela.Transform (toGhciScript)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--debug", path] -> dispatch True path
        [path] -> dispatch False path
        _ -> usage

dispatch :: Bool -> FilePath -> IO ()
dispatch isDebug path =
    case takeExtension path of
        ".md" -> if isDebug then debugNotebook path else runNotebook path
        ".markdown" -> if isDebug then debugNotebook path else runNotebook path
        _ -> if isDebug then debug path else run path

run :: FilePath -> IO ()
run path = do
    input <- TIO.readFile path
    case parseScript path input of
        Left err -> do
            putStrLn $ "sabela: parse error:\n" ++ err
            exitFailure
        Right sf -> do
            resolved <- resolveRemotes (scriptLines sf)
            let ghci = toGhciScript resolved
            runScript (sf{scriptLines = resolved}) ghci

debug :: FilePath -> IO ()
debug path = do
    input <- TIO.readFile path
    case parseScript path input of
        Left err -> do
            putStrLn $ "sabela: parse error:\n" ++ err
            exitFailure
        Right sf -> do
            resolved <- resolveRemotes (scriptLines sf)
            let ghci = toGhciScript resolved
            TIO.putStrLn ghci

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " [--debug] <script>"
    putStrLn ""
    putStrLn "  .ghci / .hs   Run as a GHCi script"
    putStrLn "  .md           Run as a markdown notebook (output to stdout)"
    putStrLn "  --debug       Print generated GHCi script without running"
    exitFailure
