{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParserGen.Wrap
    ( AlphaNum (..)
    , alphaNumToBS
    , alphaNumParser
    ) where

import Data.Char (chr, ord)
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as C8

import ParserGen.Parser

-- can keep up to 12 characters from 0..9, A..Z
newtype AlphaNum = AlphaNum { unAlphaNum :: Int64 } deriving (Show, Eq, Enum)

alphaNumToBS :: AlphaNum -> C8.ByteString
alphaNumToBS = fst . C8.unfoldrN 12 (Just . f) . unAlphaNum
    where
        f :: Int64 -> (Char, Int64)
        f i = let rest = i `div` 36
              in case fromIntegral $ i `rem` 36 of
                    l | l >= 10 -> (chr $ l - 10 + ord 'A', rest)
                    l           -> (chr $ l + ord '0', rest)

alphaNumParser :: Int -> Parser AlphaNum
alphaNumParser size = do
        raw <- unsafeTake size
        if C8.all (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z')) raw
            then return $ AlphaNum $ C8.foldr' f 0 raw
            else fail "invalid AlphaNum"
    where
        f :: Char -> Int64 -> Int64
        f c acc | c <= '9' = 36 * acc + fromIntegral (ord c - ord '0')
        f c acc            = 36 * acc + fromIntegral (ord c - ord 'A' + 10)
