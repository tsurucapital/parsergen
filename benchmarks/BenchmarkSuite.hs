module Main where

import Criterion.Main (defaultMain)

import qualified ParserGen.Benchmarks
import qualified ParserGen.Common.Benchmarks

main :: IO ()
main = defaultMain
    [ ParserGen.Benchmarks.benchmarks
    , ParserGen.Common.Benchmarks.benchmarks
    ]
