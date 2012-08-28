module ParserGen.Benchmarks
    ( benchmarks
    ) where

import Criterion (Benchmark, bench, bgroup)

import ParserGen.Benchmarks.Dummy
import ParserGen.Benchmarks.Util

benchmarks :: Benchmark
benchmarks = bgroup "ParserGen.Benchmarks"
    [ bench "parserForDummy" $ benchParse parserForDummy dummyPacket
    ]
