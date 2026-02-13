module HScript.Transform
  ( toGhciScript
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import HScript.Parse (Line(..))

toGhciScript :: [Line] -> Text
toGhciScript = T.unlines . concatMap renderBlock . groupBlocks . map rewriteLine

rewriteLine :: Line -> Line
rewriteLine (HaskellLine t) = HaskellLine (rewriteTHSplice t)
rewriteLine l = l

-- https://discourse.haskell.org/t/injecting-variables-into-a-ghci-session/12558/2?u=mchav
rewriteTHSplice :: Text -> Text
rewriteTHSplice t =
  let stripped = T.strip t
  in case T.stripPrefix "$(" stripped of
    Just rest
      | Just inner <- T.stripSuffix ")" rest
      -> "_ = (); " <> T.strip inner
    _ -> t

data Block
  = SingleLine Line
  | MultiLine [Line]
  deriving (Show)

groupBlocks :: [Line] -> [Block]
groupBlocks = concatMap splitIOBinds . groupRaw

groupRaw :: [Line] -> [Block]
groupRaw [] = []
groupRaw (Blank : rest) = SingleLine Blank : groupRaw rest
groupRaw (GhciCommand t : rest) = SingleLine (GhciCommand t) : groupRaw rest
groupRaw ls =
  let (block, rest) = span isBlockLine ls
  in  classifyBlock block : groupRaw rest

splitIOBinds :: Block -> [Block]
splitIOBinds (MultiLine ls) = map classifyBlock (splitOn isIOLine ls)
  where
    isIOLine l = isIOorTH (lineText l)
splitIOBinds b = [b]

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs)
  | p x       = [x] : splitOn p xs
  | otherwise  = let (run, rest) = break p xs
                 in  (x : run) : splitOn p rest

isBlockLine :: Line -> Bool
isBlockLine Blank          = False
isBlockLine (GhciCommand _) = False
isBlockLine _               = True

classifyBlock :: [Line] -> Block
classifyBlock [l] = SingleLine l
classifyBlock ls  = MultiLine ls

-- | Render a block as GHCi script lines.
renderBlock :: Block -> [Text]
renderBlock (SingleLine Blank)            = [""]
renderBlock (SingleLine (GhciCommand t))  = [t]
renderBlock (SingleLine (Pragma t))       = [t]
renderBlock (SingleLine (Import t))       = [t]
renderBlock (SingleLine (RemoteScript t)) = [t]
renderBlock (SingleLine (HaskellLine t))
  | isIOorTH t = wrapMulti [t]
  | otherwise   = [t]
renderBlock (MultiLine ls)
  | allIOorTH ls = concatMap (\l -> wrapMulti [lineText l]) ls
  | otherwise     = wrapMulti (map lineText ls)

wrapMulti :: [Text] -> [Text]
wrapMulti ls = [":{"] ++ ls ++ [":}"]

lineText :: Line -> Text
lineText Blank              = ""
lineText (GhciCommand t)    = t
lineText (Pragma t)         = t
lineText (Import t)         = t
lineText (RemoteScript t)   = "-- :remoteScript " <> t
lineText (HaskellLine t)    = t

isIOorTH :: Text -> Bool
isIOorTH t =
  T.isInfixOf "<-"      t ||
  T.isInfixOf "$("      t ||
  T.isPrefixOf "_ = ();" (T.stripStart t)

allIOorTH :: [Line] -> Bool
allIOorTH = all (isIOorTH . lineText)
