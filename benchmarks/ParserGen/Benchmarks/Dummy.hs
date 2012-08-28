{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParserGen.Benchmarks.Dummy
    ( Dummy (..)
    , parserForDummy
    , dummyPacket
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData, rnf)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import ParserGen.Gen
import ParserGen.Parser
import ParserGen.Common

-- Dummy types

newtype IssueCode = IssueCode ByteString deriving (Eq, Show)
newtype Price = Price Int deriving (Eq, NFData, Num, Show)
type Qty = Int
type Position = (Price, Qty)
type DiffTime = (Int, Int, Int)
data MarketStatus
    = OpeningAuction
    | RegularSession
    | ClosingAuction
    | EndOfDay
    | StatusInfo String
    deriving (Eq, Show)

-- Parsers for these dummy types

bidOrOfferOpt :: Parser Position
bidOrOfferOpt = (,) <$> unsafePriceX 5 <*> unsafeDecimalX 7

unsafePriceX :: Int -> Parser Price
unsafePriceX i = fromIntegral <$> unsafeDecimalX i

ts8 :: Parser DiffTime
ts8 = (,,) <$> unsafeDecimalX 2 <*> unsafeDecimalX 2 <*> unsafeDecimalX 4

marketStatus :: Parser MarketStatus
marketStatus = do
    n <- unsafeDecimalX 2
    return $ case n of
         0  -> OpeningAuction
         10 -> OpeningAuction
         11 -> RegularSession
         20 -> RegularSession
         21 -> RegularSession
         30 -> ClosingAuction
         40 -> RegularSession
         99 -> EndOfDay
         _  -> StatusInfo $ "Unknown market status: " ++ show n

-- Generate datypes, parsers

$(genDataTypeFromFile "Dummy.ths")
$(genParserFromFile   "Dummy.ths")

-- NFData instances for benchmark

instance NFData Dummy where
    rnf (Dummy ic cp tv tt bb ba nbb nba) =
        rnf ic  `seq`
        rnf cp  `seq`
        rnf tv  `seq`
        rnf tt  `seq`
        rnf bb  `seq`
        rnf ba  `seq`
        rnf nbb `seq`
        rnf nba

instance NFData IssueCode where
    rnf (IssueCode bs) = rnf (B.unpack bs)

dummyPacket :: ByteString
dummyPacket = B.concat
    [ "G7034"
    , "ISSUECODE   "
    , "SEQ"
    , "12345"
    , "1234567"
    , "TC"
    , "09332211"
    , "12345"
    , "12500"
    , "12300"
    , "12344"
    , "12345678"
    , "12345678901"
    , "11"  -- Market status
    , "1234567"
    , B.concat (replicate 5 "123456789000")
    , "1234567"
    , B.concat (replicate 5 "123456789000")
    , "12345"
    , B.concat (replicate 5 "1234")
    , "12345"
    , B.concat (replicate 5 "1234")
    , "\255"
    ]
