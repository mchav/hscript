module Test.Parse (parseTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Sabela.Parse

parseTests :: TestTree
parseTests =
    testGroup
        "Parse"
        [ testGroup
            "Line classification"
            [ testCase "blank line" $ do
                let sf = either error id $ parseScript "<test>" "\n"
                scriptLines sf @?= [Blank]
            , testCase "import" $ do
                let sf = either error id $ parseScript "<test>" "import Data.Text (Text)\n"
                case scriptLines sf of
                    [Import t] -> assertBool "import text" (T.isPrefixOf "import " t)
                    other -> assertFailure $ "expected Import, got: " ++ show other
            , testCase "qualified import" $ do
                let sf = either error id $ parseScript "<test>" "import qualified Data.Map as Map\n"
                case scriptLines sf of
                    [Import t] -> assertBool "qualified" (T.isInfixOf "qualified" t)
                    other -> assertFailure $ "expected Import, got: " ++ show other
            , testCase "ghci command :set" $ do
                let sf = either error id $ parseScript "<test>" ":set -XOverloadedStrings\n"
                case scriptLines sf of
                    [GhciCommand t] -> t @?= ":set -XOverloadedStrings"
                    other -> assertFailure $ "expected GhciCommand, got: " ++ show other
            , testCase "ghci command :def!" $ do
                let sf =
                        either error id $ parseScript "<test>" ":def! declareColumns \\s -> return s\n"
                case scriptLines sf of
                    [GhciCommand t] -> assertBool ":def!" (T.isPrefixOf ":def!" t)
                    other -> assertFailure $ "expected GhciCommand, got: " ++ show other
            , testCase "pragma" $ do
                let sf = either error id $ parseScript "<test>" "{-# LANGUAGE TemplateHaskell #-}\n"
                case scriptLines sf of
                    [Pragma t] -> assertBool "pragma" (T.isPrefixOf "{-#" t)
                    other -> assertFailure $ "expected Pragma, got: " ++ show other
            , testCase "remoteScript" $ do
                let sf =
                        either error id $
                            parseScript "<test>" ":remoteScript https://example.com/foo.ghci\n"
                case scriptLines sf of
                    [RemoteScript url] -> url @?= "https://example.com/foo.ghci"
                    other -> assertFailure $ "expected RemoteScript, got: " ++ show other
            , testCase "haskell line" $ do
                let sf = either error id $ parseScript "<test>" "print (5 + 5)\n"
                case scriptLines sf of
                    [HaskellLine t] -> t @?= "print (5 + 5)"
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "IO bind line" $ do
                let sf = either error id $ parseScript "<test>" "x <- getLine\n"
                case scriptLines sf of
                    [HaskellLine t] -> assertBool "has <-" (T.isInfixOf "<-" t)
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "TH splice line" $ do
                let sf = either error id $ parseScript "<test>" "$(declareColumns iris)\n"
                case scriptLines sf of
                    [HaskellLine t] -> assertBool "has $(" (T.isInfixOf "$(" t)
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            ]
        , testGroup
            "Cabal metadata"
            [ testCase "build-depends" $ do
                let sf =
                        either error id $
                            parseScript "<test>" "-- cabal: build-depends: base, text, containers\n"
                metaDeps (scriptMeta sf) @?= ["base", "text", "containers"]
            , testCase "default-extensions" $ do
                let sf =
                        either error id $
                            parseScript
                                "<test>"
                                "-- cabal: default-extensions: TemplateHaskell, TypeApplications\n"
                metaExts (scriptMeta sf) @?= ["TemplateHaskell", "TypeApplications"]
            , testCase "ghc-options" $ do
                let sf =
                        either error id $ parseScript "<test>" "-- cabal: ghc-options: -threaded, -O2\n"
                metaGhcOptions (scriptMeta sf) @?= ["-threaded", "-O2"]
            , testCase "metadata stripped from lines" $ do
                let input =
                        T.unlines
                            [ "-- cabal: build-depends: base"
                            , "import Data.Text"
                            ]
                let sf = either error id (parseScript "<test>" input)
                length (scriptLines sf) @?= 1
                case scriptLines sf of
                    [Import _] -> pure ()
                    other -> assertFailure $ "expected [Import], got: " ++ show other
            , testCase "multiple metadata lines merge" $ do
                let input =
                        T.unlines
                            [ "-- cabal: build-depends: base, text"
                            , "-- cabal: build-depends: containers"
                            , "-- cabal: default-extensions: GADTs"
                            ]
                let sf = either error id (parseScript "<test>" input)
                metaDeps (scriptMeta sf) @?= ["base", "text", "containers"]
                metaExts (scriptMeta sf) @?= ["GADTs"]
            , testCase "unknown cabal key is ignored" $ do
                let sf = either error id $ parseScript "<test>" "-- cabal: foo: bar, baz\n"
                metaDeps (scriptMeta sf) @?= []
                metaExts (scriptMeta sf) @?= []
            ]
        , testGroup
            "Multi-line scripts"
            [ testCase "interleaved imports and expressions" $ do
                let input =
                        T.unlines
                            [ "import Data.Text (Text)"
                            , ""
                            , "x <- getLine"
                            , ""
                            , "import Data.Map (Map)"
                            , ""
                            , "print x"
                            ]
                let sf = either error id (parseScript "<test>" input)
                let ls = scriptLines sf
                length ls @?= 7 -- 3 code + 4 blanks (trailing newline)
                case filter notBlank ls of
                    [Import _, HaskellLine _, Import _, HaskellLine _] -> pure ()
                    other -> assertFailure $ "unexpected structure: " ++ show other
            , testCase "empty input" $ do
                let sf = either error id $ parseScript "<test>" ""
                scriptLines sf @?= []
                metaDeps (scriptMeta sf) @?= []
            , testCase "no trailing newline" $ do
                let sf = either error id $ parseScript "<test>" "print 42"
                case scriptLines sf of
                    [HaskellLine t] -> t @?= "print 42"
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            ]
        , testGroup
            "Edge cases"
            [ testCase "comment that looks like cabal but isn't" $ do
                let sf = either error id $ parseScript "<test>" "-- cabal is great\n"

                case scriptLines sf of
                    [HaskellLine _] -> pure ()
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "regular comment" $ do
                let sf = either error id $ parseScript "<test>" "-- this is a comment\n"
                case scriptLines sf of
                    [HaskellLine t] -> assertBool "comment" (T.isPrefixOf "--" t)
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "indented import is haskell line" $ do
                let sf = either error id $ parseScript "<test>" "  import Data.Text\n"
                case scriptLines sf of
                    [HaskellLine _] -> pure ()
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            ]
        ]

notBlank :: Line -> Bool
notBlank Blank = False
notBlank _ = True
