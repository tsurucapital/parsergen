module ParserGen.Benchmarks.Util
    ( benchParse
    ) where

import Control.DeepSeq (NFData)
import Criterion (Pure, nf)
import Data.ByteString (ByteString)

import ParserGen.Parser (Parser, parse)

benchParse :: NFData a => Parser a -> ByteString -> Pure
benchParse parser bs = nf (parse parser) bs
