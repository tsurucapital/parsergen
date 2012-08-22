{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.DeepSeq (NFData)
import Criterion (Pure, bench, nf)
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import ParserGen.Parser

main :: IO ()
main = defaultMain
    [ bench "unsafeDecimalX" $ benchParse (unsafeDecimalX 10) input
    ]

benchParse :: NFData a => Parser a -> ByteString -> Pure
benchParse parser bs = nf (parse parser) bs

input :: ByteString
input = B.concat $ replicate 100 "1234"
