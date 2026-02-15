module Test.Integration (integrationTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Parse (CabalMeta (..), ScriptFile (..), parseScript)
import Sabela.Transform (toGhciScript)

integrationTests :: TestTree
integrationTests =
    testGroup
        "Integration (Parse → Transform)"
        [ testCase "typical dataframe script" $ do
            let input =
                    T.unlines
                        [ "-- cabal: build-depends: base, dataframe, text"
                        , "-- cabal: default-extensions: TemplateHaskell, TypeApplications"
                        , ""
                        , "import qualified DataFrame as D"
                        , ""
                        , "iris <- D.readParquet \"data/iris.parquet\""
                        , ""
                        , "import Data.Text (Text)"
                        , ""
                        , "$(F.declareColumns iris)"
                        , ""
                        , "D.derive \"ratio\" (sepal_width / sepal_length) iris"
                        ]
            let sf = either error id (parseScript "<test>" input)

            metaDeps (scriptMeta sf) @?= ["base", "dataframe", "text"]
            metaExts (scriptMeta sf) @?= ["TemplateHaskell", "TypeApplications"]

            let ghci = toGhciScript (scriptLines sf)

            assertBool "no -- cabal:" (not $ T.isInfixOf "-- cabal:" ghci)

            assertBool "has import D" (T.isInfixOf "import qualified DataFrame as D" ghci)
            assertBool "has import Text" (T.isInfixOf "import Data.Text (Text)" ghci)

            assertBool "iris wrapped" (hasWrappedLine "iris <- D.readParquet" ghci)

            assertBool "TH rewritten" (T.isInfixOf "_ = (); F.declareColumns iris" ghci)
            assertBool "no $(" (not $ T.isInfixOf "$(" ghci)
        , testCase "script with ghci commands" $ do
            let input =
                    T.unlines
                        [ ":set -XOverloadedStrings"
                        , "import Data.Text (Text)"
                        , "default (Int, Text)"
                        , ""
                        , "print (\"hello\" :: Text)"
                        ]
            let sf = either error id (parseScript "<test>" input)
            let ghci = toGhciScript (scriptLines sf)

            assertBool "has :set" (T.isInfixOf ":set -XOverloadedStrings" ghci)

            assertBool "has import" (T.isInfixOf "import Data.Text" ghci)
        , testCase "getNumProcessors pattern splits correctly" $ do
            let input =
                    T.unlines
                        [ "import GHC.Conc"
                        , ""
                        , "n <- getNumProcessors"
                        , "setNumCapabilities $ max 1 (min 4 (n-1))"
                        ]
            let sf = either error id (parseScript "<test>" input)
            let ghci = toGhciScript (scriptLines sf)

            assertBool "n wrapped" (hasWrappedLine "n <- getNumProcessors" ghci)

            let blocks = extractBlocks ghci
            let hasN = any (any (T.isInfixOf "n <- getNumProcessors")) blocks
            assertBool "n in a block" hasN
        , testCase "empty script" $ do
            let sf = either error id $ parseScript "<test>" ""
            let ghci = toGhciScript (scriptLines sf)
            assertBool "empty or whitespace" (T.null (T.strip ghci))
        , testCase "multiple blank lines preserved as separators" $ do
            let input =
                    T.unlines
                        [ "print 1"
                        , ""
                        , ""
                        , "print 2"
                        ]
            let sf = either error id (parseScript "<test>" input)
            let ghci = toGhciScript (scriptLines sf)
            assertBool "has print 1" (T.isInfixOf "print 1" ghci)
            assertBool "has print 2" (T.isInfixOf "print 2" ghci)
        , testCase "pipe chain kept together" $ do
            let input =
                    T.unlines
                        [ "iris |>"
                        , "  D.filterWhere (variety .== \"Setosa\") |>"
                        , "  D.filterWhere (sepal_length .> 5.4)"
                        ]
            let sf = either error id (parseScript "<test>" input)
            let ghci = toGhciScript (scriptLines sf)

            let blocks = extractBlocks ghci
            let pipeBlock = filter (any (T.isInfixOf "iris |>")) blocks
            assertBool "pipe chain in one block" (length pipeBlock == 1)
            case pipeBlock of
                [b] -> do
                    assertBool "has filterWhere" (any (T.isInfixOf "D.filterWhere") b)
                _ -> pure ()
        ]

hasWrappedLine :: Text -> Text -> Bool
hasWrappedLine needle ghci =
    let blocks = extractBlocks ghci
     in any (any (T.isInfixOf needle)) blocks

extractBlocks :: Text -> [[Text]]
extractBlocks = go . T.lines
  where
    go [] = []
    go (l : ls)
        | T.strip l == ":{" =
            let (block, rest) = span (\x -> T.strip x /= ":}") ls
                rest' = drop 1 rest
             in block : go rest'
        | otherwise = go ls
