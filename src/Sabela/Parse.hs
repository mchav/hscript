module Sabela.Parse (
    ScriptFile (..),
    CabalMeta (..),
    Line (..),
    parseScript,
) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data ScriptFile = ScriptFile
    { scriptMeta :: CabalMeta
    , scriptLines :: [Line]
    }
    deriving (Show, Eq)

data CabalMeta = CabalMeta
    { metaDeps :: [Text]
    , metaExts :: [Text]
    , metaGhcOptions :: [Text]
    }
    deriving (Show, Eq)

-- | A single logical line from the script.
data Line
    = Blank
    | GhciCommand Text
    | Pragma Text
    | Import Text
    | RemoteScript Text
    | HaskellLine Text
    deriving (Show, Eq)

parseScript :: FilePath -> Text -> Either String ScriptFile
parseScript path input =
    case parse scriptFileP path input of
        Left err -> Left (errorBundlePretty err)
        Right sf -> Right sf

scriptFileP :: Parser ScriptFile
scriptFileP = do
    ls <- many lineP <* eof
    let (metas, code) = partitionLines ls
        meta = mergeMetas metas
    pure (ScriptFile meta code)

data RawLine
    = RawCabalMeta CabalMeta
    | RawCode Line

partitionLines :: [RawLine] -> ([CabalMeta], [Line])
partitionLines = foldr go ([], [])
  where
    go (RawCabalMeta m) (ms, cs) = (m : ms, cs)
    go (RawCode c) (ms, cs) = (ms, c : cs)

mergeMetas :: [CabalMeta] -> CabalMeta
mergeMetas ms =
    CabalMeta
        { metaDeps = concatMap metaDeps ms
        , metaExts = concatMap metaExts ms
        , metaGhcOptions = concatMap metaGhcOptions ms
        }

lineP :: Parser RawLine
lineP = do
    notFollowedBy eof
    choice
        [ RawCabalMeta <$> try cabalMetaP
        , RawCode <$> codeLineP
        ]
        <?> "line"

cabalMetaP :: Parser CabalMeta
cabalMetaP = do
    _ <- string "-- cabal:"
    hspace
    key <- takeWhile1P (Just "key") (/= ':')
    _ <- char ':'
    hspace
    values <- restOfLine
    let items = map T.strip (T.splitOn "," values)
    pure $ case T.strip key of
        "build-depends" -> emptyCabal{metaDeps = items}
        "default-extensions" -> emptyCabal{metaExts = items}
        "ghc-options" -> emptyCabal{metaGhcOptions = items}
        _ -> emptyCabal
  where
    emptyCabal = CabalMeta [] [] []

codeLineP :: Parser Line
codeLineP =
    choice
        [ Blank <$ try blankLineP
        , RemoteScript <$> try remoteScriptP
        , GhciCommand <$> try ghciCommandP
        , Pragma <$> try pragmaP
        , Import <$> try importP
        , HaskellLine <$> restOfLine
        ]

remoteScriptP :: Parser Text
remoteScriptP = do
    hspace
    _ <- string ":remoteScript"
    hspace1
    T.strip <$> restOfLine

blankLineP :: Parser ()
blankLineP = hspace *> void newline

ghciCommandP :: Parser Text
ghciCommandP = do
    hspace
    _ <- char ':'
    rest <- restOfLine
    pure (":" <> rest)

pragmaP :: Parser Text
pragmaP = do
    s <- string "{-#"
    rest <- restOfLine
    pure (s <> rest)

importP :: Parser Text
importP = do
    s <- string "import"
    c <- satisfy (\ch -> ch == ' ' || ch == '\t')
    rest <- restOfLine
    pure (s <> T.singleton c <> rest)

restOfLine :: Parser Text
restOfLine = do
    t <- takeWhileP Nothing (/= '\n')
    _ <- void newline <|> eof
    pure t
