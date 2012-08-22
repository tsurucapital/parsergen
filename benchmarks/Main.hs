{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.DeepSeq (NFData)
import Criterion (Pure, bench, nf)
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import ParserGen.Common
import ParserGen.Parser

main :: IO ()
main = defaultMain
    [ bench "unsafeDecimalX 4"       $ benchParse (unsafeDecimalX 4)     i1
    , bench "unsafeDecimalX 4 (TH)"  $ benchParse $(unsafeDecimalXTH 4)  i1
    , bench "unsafeDecimalX 10"      $ benchParse (unsafeDecimalX 10)    i1
    , bench "unsafeDecimalX 10 (TH)" $ benchParse $(unsafeDecimalXTH 10) i1

    , bench "unsafeDecimalXS 4"       $ benchParse (unsafeDecimalXS 4)     i2
    , bench "unsafeDecimalXS 4 (TH)"  $ benchParse $(unsafeDecimalXSTH 4)  i2
    , bench "unsafeDecimalXS 10"      $ benchParse (unsafeDecimalXS 10)    i2
    , bench "unsafeDecimalXS 10 (TH)" $ benchParse $(unsafeDecimalXSTH 10) i2
    ]

benchParse :: NFData a => Parser a -> ByteString -> Pure
benchParse parser bs = nf (parse parser) bs

i1 :: ByteString
i1 = B.concat $ replicate 100 "1234"
{-# NOINLINE i1 #-}

i2 :: ByteString
i2 = B.concat $ "-" : replicate 100 "1234"
{-# NOINLINE i2 #-}
