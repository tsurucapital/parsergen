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
    [ bench "unsafeDecimalX 10"      $ benchParse (unsafeDecimalX 10)    input
    , bench "unsafeDecimalX 10 (TH)" $ benchParse $(unsafeDecimalXTH 10) input
    , bench "unsafeDecimalX 4"       $ benchParse (unsafeDecimalX 4)     input
    , bench "unsafeDecimalX 4 (TH)"  $ benchParse $(unsafeDecimalXTH 4)  input
    ]

benchParse :: NFData a => Parser a -> ByteString -> Pure
benchParse parser bs = nf (parse parser) bs

input :: ByteString
input = B.concat $ replicate 100 "1234"
