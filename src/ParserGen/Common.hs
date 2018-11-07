-- | Parsing and unparsing for commonly used datatypes
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

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

    , StringPattern(..)
    , stringPatternTH
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Unsafe as B
import Data.Char (chr, ord)
import Data.Int (Int64)
import Language.Haskell.TH

import ParserGen.Parser (Parser)
import qualified ParserGen.Parser as P

import Data.Bits
import Data.Word
import Language.Haskell.TH.Syntax (Lift)
import Foreign.Storable
import Foreign.ForeignPtr
import Data.ByteString.Internal (accursedUnutterablePerformIO, ByteString(..))

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
                    then fail $ "Not an Int: " ++ show bs
                    else loop (acc * 10 - ord '0' + x) (i + 1)
    {-# INLINE go #-}
{-# INLINE unsafeDecimalX #-}

-- | This is a template-haskell based version of 'unsafeDecimalX' which
-- generates a fast, unrolled loop
unsafeDecimalXTH :: Int -> Q Exp
unsafeDecimalXTH 0    = [|return (0 :: Int)|]
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
            xv   <- [|fromIntegral (subtract 48 $ B.unsafeIndex $(varE bs) i) :: Int|]
            accv <- if prevacc == (LitE (IntegerL 0))
                        then varE x
                        else [| $(return prevacc) * 10 + $(varE x) |]
            next <- go bs (VarE acc) (i + 1)

            body <- [| if $(varE x) >= 10
                        then fail $ "Not an Int: " ++ show $(varE bs)
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


newtype StringPattern = StringPattern ()

stringPatternTH :: Int -> Int -> String -> Q Exp
stringPatternTH b a p@(length -> l) = [| $(stringPatternMatch b a p) >> P.unsafeSkip l |]


stringPatternMatch :: Int -> Int -> String -> Q Exp
stringPatternMatch _before _after pat@(length -> l)
    | l == 0 = [| return () |]
    | l == 1 = let (expected :: Word8, mask :: Word8, _) = takeMask (extractMask pat)
                in maskedTH ''Word8 (expected :: Word8) (mask :: Word8)   1
    | l == 2 = let (expected :: Word16, mask :: Word16, _) = takeMask (extractMask pat)
                in maskedTH ''Word16 (expected :: Word16) (mask :: Word16) 2
    | l == 4 = let (expected :: Word32, mask :: Word32, _) = takeMask (extractMask pat)
                in maskedTH ''Word32 (expected :: Word32) (mask :: Word32) 4
    | l == 8 = let (expected :: Word64, mask :: Word64, _) = takeMask (extractMask pat)
                in maskedTH ''Word64 (expected :: Word64) (mask :: Word64) 8
    | l > 8 = do let (xs, rest) = splitAt 8 pat
                 this <- stringPatternMatch _before _after xs
                 next <- stringPatternMatch (_before + 8) (_after - 8) rest
                 [| $( return this ) >> $( return next ) |]
    | l > 4 && l < 8 && l + _after >= 8 =
                let pat' = take 8 $ pat ++ repeat '?'
                 in stringPatternMatch _before (_after - l) pat'
    | otherwise = error $ "Unhandled pat: " ++ show (_before, _after, pat, l)

takeMask :: forall a. (Num a, Storable a, FiniteBits a) => [(Word8, Bool)] -> (a, a, [(Word8, Bool)])
takeMask xs =
    let width = finiteBitSize (undefined :: a) `div` 8
        (ys, rest) = splitAt width xs
        ys' = take width (ys ++ repeat (0, False))
        !pat = foldr (\(p, _) acc -> acc `shiftL` 8 + fromIntegral p) 0 ys'
        !mask = foldr (\(_, m) acc -> acc `shiftL` 8 + if m then 0xFF else 0x00) 0 ys'
     in (pat, mask, rest)


extractMask :: String -> [(Word8, Bool)]
extractMask []             = []
extractMask ('\\':c@'?' :xs) = (fromIntegral $ ord c, True) : extractMask xs
extractMask ('\\':c@'\\':xs) = (fromIntegral $ ord c, True) : extractMask xs
extractMask ('?'        :xs) = (0, False)                   : extractMask xs
extractMask (c          :xs) = (fromIntegral $ ord c, True) : extractMask xs

maskedTH :: (Storable a, FiniteBits a, Lift a) => Name -> a -> a -> Int -> Q Exp
maskedTH n expected mask s = [| P.peekBS >>= matchMasked (expected :: $x) (mask :: $x) |]
    where
        x = conT n

{-# INLINE matchMasked #-}
matchMasked :: (Storable a, FiniteBits a) => a -> a -> ByteString -> Parser ()
matchMasked expect mask (PS x s _l) =
    if (accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
            (expect /=) . (mask .&.) <$> peekByteOff p s)
        then fail "match failed"
        else return ()



unsafeAlphaNum :: Int -> Parser AlphaNum
unsafeAlphaNum l = P.unsafeTake l >>= go
  where
    go bs = loop 0 0
      where
        fail' = fail $ "Invalid AlphaNum: " ++ show bs
        -- We assume some things about the ascii layout, and get better
        -- branching that way...
        loop !acc !i
            | i >= l       =
                return $ AlphaNum acc
            | w <= c2w '9' = if w < c2w '0'
                then fail'
                else loop (36 * acc + (fromIntegral $ w - c2w '0')) (i + 1)
            | otherwise    = if w < c2w 'A' || w > c2w 'Z'
                then fail'
                else loop (36 * acc + (fromIntegral $ w - c2w 'A' + 10)) (i + 1)
          where
            w = B.unsafeIndex bs i
    {-# INLINE go #-}
{-# INLINE unsafeAlphaNum #-}

putAlphaNum :: AlphaNum -> ByteString
putAlphaNum (AlphaNum an) = fst $ BC.unfoldrN 12 f (36 ^ (11 :: Int))
  where
    f :: Int64 -> Maybe (Char, Int64)
    f i | i <= 0    = Nothing
        | l >= 10   = Just (chr $ l - 10 + ord 'A', i `div` 36)
        | otherwise = Just (chr $ l + ord '0', i `div` 36)
      where
        -- Expensive? :-(
        l = fromIntegral $ (an `div` i) `mod` 36
    {-# INLINE f #-}
{-# INLINE putAlphaNum #-}
