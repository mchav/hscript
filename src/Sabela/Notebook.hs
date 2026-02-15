module Sabela.Notebook (
    runNotebook,
    debugNotebook,
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

import Sabela.Markdown (Segment (..), parseMarkdown, reassemble)
import Sabela.Parse (CabalMeta (..), Line (..), ScriptFile (..), parseScript)
import Sabela.Resolve (resolveRemotes)
import Sabela.Run (runScriptCapture)
import Sabela.Transform (toGhciScript)

marker :: Int -> Text
marker n = "---HSCRIPT_BLOCK_" <> T.pack (show n) <> "_END---"

runNotebook :: FilePath -> IO ()
runNotebook path = do
    input <- TIO.readFile path
    (outputMd, _) <- processNotebook input
    TIO.putStr outputMd

debugNotebook :: FilePath -> IO ()
debugNotebook path = do
    input <- TIO.readFile path
    (_, ghciScript) <- processNotebook' input
    TIO.putStrLn ghciScript

processNotebook :: Text -> IO (Text, Text)
processNotebook input = do
    let segments = parseMarkdown input
        haskellBlocks =
            [ (i, code)
            | (i, CodeBlock lang code) <- zip [0 ..] segments
            , isHaskell lang
            ]

    if null haskellBlocks
        then pure (input, "")
        else do
            (meta, blockLines) <- parseAndResolveBlocks haskellBlocks

            let ghciScript = generateMarkedScript blockLines

            let sf = ScriptFile meta []
            rawOutput <- runScriptCapture sf ghciScript

            let blockIndices = map fst haskellBlocks
                outputs = splitByMarkers blockIndices rawOutput

            let outputMap = zip (map fst haskellBlocks) outputs

            let pairs = zipWith (attachOutput outputMap) [0 ..] segments
            pure (reassemble pairs, rawOutput)

processNotebook' :: Text -> IO (Text, Text)
processNotebook' input = do
    let segments = parseMarkdown input
        haskellBlocks =
            [ (i, code)
            | (i, CodeBlock lang code) <- zip [0 ..] segments
            , isHaskell lang
            ]

    if null haskellBlocks
        then pure (input, "")
        else do
            (_, blockLines) <- parseAndResolveBlocks haskellBlocks
            let ghciScript = generateMarkedScript blockLines
            pure (input, ghciScript)

isHaskell :: Text -> Bool
isHaskell lang = T.toLower (T.strip lang) `elem` ["haskell", "hs"]

parseAndResolveBlocks :: [(Int, Text)] -> IO (CabalMeta, [([Line], Int)])
parseAndResolveBlocks blocks = do
    results <- mapM parseBlock blocks
    let allMetas = map (\(m, _, _) -> m) results
        merged = mergeMetas allMetas
        blockLines = map (\(_, ls, idx) -> (ls, idx)) results
    pure (merged, blockLines)

parseBlock :: (Int, Text) -> IO (CabalMeta, [Line], Int)
parseBlock (idx, code) = do
    case parseScript ("<block " ++ show idx ++ ">") code of
        Left err -> do
            putStrLn $ "sabela: error in code block " ++ show idx ++ ":\n" ++ err
            exitFailure
        Right sf -> do
            resolved <- resolveRemotes (scriptLines sf)
            pure (scriptMeta sf, resolved, idx)

mergeMetas :: [CabalMeta] -> CabalMeta
mergeMetas ms =
    CabalMeta
        { metaDeps = concatMap metaDeps ms
        , metaExts = concatMap metaExts ms
        , metaGhcOptions = concatMap metaGhcOptions ms
        }

generateMarkedScript :: [([Line], Int)] -> Text
generateMarkedScript blocks =
    T.unlines $ concatMap renderWithMarker blocks
  where
    renderWithMarker (ls, idx) =
        let ghciLines = T.lines (toGhciScript ls)
            markerLine = "putStrLn " <> T.pack (show (T.unpack (marker idx)))
         in ghciLines ++ [markerLine]

splitByMarkers :: [Int] -> Text -> [Text]
splitByMarkers [] _ = []
splitByMarkers (idx : rest) remaining =
    let mk = marker idx
        (before, after) = T.breakOn mk remaining
     in if T.null after
            then [T.strip before]
            else T.strip before : splitByMarkers rest (T.drop (T.length mk) after)

attachOutput :: [(Int, Text)] -> Int -> Segment -> (Segment, Maybe Text)
attachOutput outputMap idx seg =
    case lookup idx outputMap of
        Just out | not (T.null (T.strip out)) -> (seg, Just out)
        _ -> (seg, Nothing)
