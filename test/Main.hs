module Main (main) where

import Test.Tasty

import Test.Integration (integrationTests)
import Test.Markdown (markdownTests)
import Test.Parse (parseTests)
import Test.Transform (transformTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Sabela"
            [ parseTests
            , transformTests
            , markdownTests
            , integrationTests
            ]
