module Sabela.Markdown (
    Segment (..),
    parseMarkdown,
    reassemble,
) where

import Data.Text (Text)
import qualified Data.Text as T

data Segment
    = Prose Text
    | CodeBlock Text Text
    deriving (Show)

parseMarkdown :: Text -> [Segment]
parseMarkdown = go [] . T.lines
  where
    go acc [] =
        let prose = flushProse acc
         in [Prose prose | not (T.null prose)]
    go acc (line : rest)
        | Just lang <- parseFenceOpen line =
            let prose = flushProse acc
                (codeLines, rest') = spanUntilFenceClose rest
                code = T.unlines codeLines
                segments =
                    [Prose prose | not (T.null prose)]
                        ++ [CodeBlock lang code]
             in segments ++ go [] rest'
        | otherwise = go (acc ++ [line]) rest

    flushProse ls
        | null ls = T.empty
        | otherwise = T.unlines ls

parseFenceOpen :: Text -> Maybe Text
parseFenceOpen line =
    let stripped = T.strip line
     in case T.stripPrefix "```" stripped of
            Just rest
                | not (T.null rest)
                , T.all (/= '`') rest ->
                    Just (T.strip rest)
            _ -> Nothing

spanUntilFenceClose :: [Text] -> ([Text], [Text])
spanUntilFenceClose [] = ([], [])
spanUntilFenceClose (line : rest)
    | isFenceClose line = ([], rest)
    | otherwise =
        let (code, rest') = spanUntilFenceClose rest
         in (line : code, rest')

isFenceClose :: Text -> Bool
isFenceClose line =
    let stripped = T.strip line
     in stripped == "```"

reassemble :: [(Segment, Maybe Text)] -> Text
reassemble = T.concat . map renderPair

renderPair :: (Segment, Maybe Text) -> Text
renderPair (Prose t, _) = t
renderPair (CodeBlock lang code, mOutput) =
    let fence = "\n```" <> lang <> "\n" <> code <> "\n```\n"
        output = case mOutput of
            Nothing -> ""
            Just out
                | T.null (T.strip out) -> ""
                | otherwise -> blockquote out <> "\n"
     in fence <> output

blockquote :: Text -> Text
blockquote t =
    let ls = T.lines t
        trimmed = reverse $ dropWhile T.null $ reverse ls
     in T.unlines $ map (\l -> if T.null l then ">" else "> " <> l) trimmed
