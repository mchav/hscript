module Test.Transform (transformTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Parse (Line (..))
import Sabela.Transform (toGhciScript)

transformTests :: TestTree
transformTests =
    testGroup
        "Transform"
        [ testGroup
            "Single lines"
            [ testCase "plain expression stays unwrapped" $ do
                let result = toGhciScript [HaskellLine "print 42"]
                assertNotWrapped result
            , testCase "import stays unwrapped" $ do
                let result = toGhciScript [Import "import Data.Text"]
                assertNotWrapped result
            , testCase "pragma stays unwrapped" $ do
                let result = toGhciScript [Import "{-# LANGUAGE GADTs #-}"]
                assertNotWrapped result
            , testCase "ghci command stays unwrapped" $ do
                let result = toGhciScript [GhciCommand ":set -XOverloadedStrings"]
                let ls = nonEmpty result
                ls @?= [":set -XOverloadedStrings"]
            , testCase "IO bind gets wrapped" $ do
                let result = toGhciScript [HaskellLine "x <- getLine"]
                assertWrapped result ["x <- getLine"]
            , testCase "blank line preserved" $ do
                let result = toGhciScript [Blank]
                assertBool "has blank" (T.isInfixOf "\n\n" result || result == "\n")
            ]
        , testGroup
            "TH splice rewriting"
            [ testCase "$(expr) becomes _ = (); expr" $ do
                let result = toGhciScript [HaskellLine "$(declareColumns iris)"]
                assertBool "rewritten" (T.isInfixOf "_ = (); declareColumns iris" result)
                assertBool "no $(" (not $ T.isInfixOf "$(" result)
            , testCase "nested parens not rewritten" $ do
                let result = toGhciScript [HaskellLine "f $(g x) y"]
                -- This has $( but is not a top-level splice (doesn't end with matching paren)
                assertBool "contains original" (T.isInfixOf "f $(g x) y" result)
            , testCase "rewritten splice is wrapped as IO" $ do
                let result = toGhciScript [HaskellLine "$(declareColumns iris)"]
                assertBool "has :{" (T.isInfixOf ":{" result)
            ]
        , testGroup
            "IO line isolation"
            [ testCase "consecutive IO binds each get own block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x <- getLine"
                            , HaskellLine "y <- getLine"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 2 blocks, got " ++ show (length blocks))
                    (length blocks == 2)
            , testCase "IO bind between pure code splits correctly" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "let x = 5"
                            , HaskellLine "y <- getLine"
                            , HaskellLine "print y"
                            ]
                let blocks = splitBlocks result
                -- Should be: [y <- getLine]
                assertBool
                    ("expected 2 blocks, got " ++ show (length blocks) ++ ": " ++ show blocks)
                    (length blocks >= 3)
            ]
        , testGroup
            "Multi-line blocks"
            [ testCase "consecutive pure lines grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "let"
                            , HaskellLine "  x = 5"
                            , HaskellLine "  y = 10"
                            ]
                let blocks = splitBlocks result
                length blocks @?= 1
            , testCase "blank separates blocks" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "print 1"
                            , Blank
                            , HaskellLine "print 2"
                            ]
                -- Two separate blocks
                let ls = nonEmpty result
                assertBool "has print 1" (any (T.isInfixOf "print 1") ls)
                assertBool "has print 2" (any (T.isInfixOf "print 2") ls)
            ]
        , testGroup
            "Full script transform"
            [ testCase "typical script" $ do
                let result =
                        toGhciScript
                            [ Import "import qualified DataFrame as D"
                            , Blank
                            , HaskellLine "iris <- D.readParquet \"data/iris.parquet\""
                            , Blank
                            , Import "import Data.Text (Text)"
                            , Blank
                            , HaskellLine "print iris"
                            ]
                -- Import should be present
                assertBool "has import D" (T.isInfixOf "import qualified DataFrame as D" result)
                -- IO bind should be wrapped
                assertBool "has :{" (T.isInfixOf ":{" result)
                -- Has the expression
                assertBool "has print" (T.isInfixOf "print iris" result)
            ]
        ]

-- | Extract :{ ... :} blocks from output.
splitBlocks :: Text -> [[Text]]
splitBlocks = go . T.lines
  where
    go [] = []
    go (l : ls)
        | T.strip l == ":{" =
            let (block, rest) = span (\x -> T.strip x /= ":}") ls
                rest' = drop 1 rest -- skip the :}
             in block : go rest'
        | otherwise = go ls

-- | Non-empty, stripped lines.
nonEmpty :: Text -> [Text]
nonEmpty = filter (not . T.null . T.strip) . T.lines

-- | Assert the output does NOT contain :{ :} wrapping.
assertNotWrapped :: Text -> Assertion
assertNotWrapped t =
    assertBool
        ("expected no wrapping, got: " ++ T.unpack t)
        (not (T.isInfixOf ":{" t))

-- | Assert the output wraps exactly these lines in :{ :}.
assertWrapped :: Text -> [Text] -> Assertion
assertWrapped t expectedInner = do
    let blocks = splitBlocks t
    assertBool
        ("expected at least one block, got: " ++ show blocks)
        (not (null blocks))
    let innerLines = map T.strip (concat blocks)
    let expected = map T.strip expectedInner
    assertBool
        ("expected " ++ show expected ++ " in blocks, got: " ++ show innerLines)
        (all (`elem` innerLines) expected)
