{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserGen.Common.Benchmarks
    ( benchmarks
    ) where

import Control.DeepSeq (NFData (..))
import Criterion (Benchmark, bench, bgroup)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import ParserGen.Benchmarks.Util
import ParserGen.Common
import qualified ParserGen.Common.Atoi as Atoi

benchmarks :: Benchmark
benchmarks = bgroup "ParserGen.Common.Benchmarks"
    [ bench "unsafeDecimalX 4"    $ benchParse (unsafeDecimalX 4)     i1
    , bench "unsafeDecimalX 10"   $ benchParse (unsafeDecimalX 10)    i1
    , bench "unsafeDecimalXTH 4"  $ benchParse $(unsafeDecimalXTH 4)  i1
    , bench "unsafeDecimalXTH 10" $ benchParse $(unsafeDecimalXTH 10) i1

    , bench "unsafeDecimalXS 4"    $ benchParse (unsafeDecimalXS 4)     i2
    , bench "unsafeDecimalXS 10"   $ benchParse (unsafeDecimalXS 10)    i2
    , bench "unsafeDecimalXSTH 4"  $ benchParse $(unsafeDecimalXSTH 4)  i2
    , bench "unsafeDecimalXSTH 10" $ benchParse $(unsafeDecimalXSTH 10) i2

    , bench "alphaNum 12" $ benchParse (unsafeAlphaNum 12) i3

      -- Just for comparison
    , bench "atoi 4"  $ benchParse (Atoi.unsafeDecimalX 4)  i1
    , bench "atoi 10" $ benchParse (Atoi.unsafeDecimalX 10) i1
    ]

i1 :: ByteString
i1 = B.concat $ replicate 100 "1234"
{-# NOINLINE i1 #-}

i2 :: ByteString
i2 = B.concat $ "-" : replicate 100 "1234"
{-# NOINLINE i2 #-}

i3 :: ByteString
i3 = "ABC123XYZ789"
{-# NOINLINE i3 #-}

instance NFData AlphaNum where
    rnf (AlphaNum x) = rnf x
