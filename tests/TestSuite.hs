module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified ParserGen.Common.Tests
import qualified ParserGen.Tests

main :: IO ()
main = defaultMain
    [ ParserGen.Common.Tests.tests
    , ParserGen.Tests.tests
    ]
