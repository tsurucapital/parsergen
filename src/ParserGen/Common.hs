-- | Parsing and unparsing for commonly used datatypes
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Common
    ( unsafeDecimalX
    , unsafeDecimalXTH
    , putDecimalX

    , unsafeDecimalXS
    , unsafeDecimalXSTH
    , putDecimalXS

    , AlphaNum (..)
    , unsafeAlphaNum
    , putAlphaNum
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as B
import Data.Char (chr, ord)
import Data.Int (Int64)
import Language.Haskell.TH

import ParserGen.Parser (Parser)
import qualified ParserGen.Parser as P

unsafeDecimalX :: Int -> Parser Int
unsafeDecimalX l = P.unsafeTake l >>= go
  where
    go bs = loop 0 0
      where
        loop !acc !i
            | i >= l    = return acc
            | otherwise =
                let x = fromIntegral (B.unsafeIndex bs i)
                in if x < ord '0' || x > ord '9'
                    then fail $ "not an Int: " ++ show bs
                    else loop (acc * 10 - ord '0' + x) (i + 1)
    {-# INLINE go #-}
{-# INLINE unsafeDecimalX #-}

-- | This is a template-haskell based version of 'unsafeDecimalX' which
-- generates a fast, unrolled loop
unsafeDecimalXTH :: Int -> Q Exp
unsafeDecimalXTH size = do
    bs  <- newName "bs"
    go' <- go bs (LitE (IntegerL 0)) 0
    [|P.unsafeTake size >>= $(return $ LamE [VarP bs] go')|]
  where
    go :: Name -> Exp -> Int -> Q Exp
    go bs prevacc i
        | i >= size = [|return $(return prevacc)|]
        | otherwise = do
            x    <- newName $ "x" ++ show i
            acc  <- newName $ "var" ++ show i
            xv   <- [|fromIntegral (B.unsafeIndex $(varE bs) i) :: Int|]
            accv <- [|$(return prevacc) * 10 + $(varE x) - ord '0'|]
            next <- go bs (VarE acc) (i + 1)

            body <- [| if $(varE x) < ord '0' || $(varE x) > ord '9'
                        then fail $ "Not an Int " ++ show $(varE bs)
                        else $(return next) |]

            return $ LetE
                [ ValD (VarP x)           (NormalB xv)   []
                , ValD (BangP (VarP acc)) (NormalB accv) []
                ] body

putDecimalX :: Int -> Int -> ByteString
putDecimalX l i = BC.pack $ putDecimalXString l i

unsafeDecimalXS :: Int -> Parser Int
unsafeDecimalXS l = sign <*> unsafeDecimalX l
{-# INLINE unsafeDecimalXS #-}

unsafeDecimalXSTH :: Int -> Q Exp
unsafeDecimalXSTH size = [|sign <*> $(unsafeDecimalXTH size)|]

-- | Helper function
sign :: Parser (Int -> Int)
sign = do
    raw <- BC.head <$> P.unsafeTake 1
    case raw of
        '+' -> return id
        ' ' -> return id
        '0' -> return id
        '-' -> return negate
        inv -> fail $ "Invalid sign: " ++ show inv
{-# INLINE sign #-}

putDecimalXS :: Int ->  Int -> ByteString
putDecimalXS l i
    | i >= 0    = BC.pack $ ' ' : putDecimalXString l i
    | otherwise = BC.pack $ '-' : putDecimalXString l (negate i)

-- | Helper function
putDecimalXString :: Int -> Int -> String
putDecimalXString l i
    | i >= 0    = reverse . take l . reverse $ (replicate l '0' ++ show i)
    | otherwise =
        error "ParserGen.Repack: Can't put negative decimal X: " ++ show i

-- | Can keep up to 12 characters from 0..9, A..Z
newtype AlphaNum = AlphaNum {unAlphaNum :: Int64}
    deriving (Show, Eq, Enum)

unsafeAlphaNum :: Int -> Parser AlphaNum
unsafeAlphaNum size = do
    raw <- P.unsafeTake size
    if BC.all (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z')) raw
        then return $ AlphaNum $ BC.foldr' f 0 raw
        else fail "invalid AlphaNum"
  where
    f :: Char -> Int64 -> Int64
    f c acc
        | c <= '9'  = 36 * acc + fromIntegral (ord c - ord '0')
        | otherwise = 36 * acc + fromIntegral (ord c - ord 'A' + 10)
    {-# INLINE f #-}
{-# INLINE unsafeAlphaNum #-}

putAlphaNum :: AlphaNum -> ByteString
putAlphaNum = fst . BC.unfoldrN 12 (Just . f) . unAlphaNum
  where
    f :: Int64 -> (Char, Int64)
    f i = let rest = i `div` 36
          in case fromIntegral $ i `rem` 36 of
                l | l >= 10 -> (chr $ l - 10 + ord 'A', rest)
                l           -> (chr $ l + ord '0', rest)
