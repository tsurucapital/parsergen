-- | Based on Data.Attoparsec.Zepto by  Bryan O'Sullivan 2011
--
-- A tiny, highly specialized combinator parser for 'B.ByteString'
-- strings. Designed to split bytestrings into fields with fixed widths.
--
-- unsafe versions of the functions do not perform checks that there
-- is enough data left in the bytestring
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -ddump-simpl -ddump-to-file -ddump-asm #-}
module ParserGen.Parser
    ( Parser
    , parse
    , consumeWord8
    , unsafeTake
    , unsafeSkip
    , ensureBytesLeft
    , string
    , anyChar
    , take
    , skip
    ) where

import Data.Word (Word8)
import Control.Applicative
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peekByteOff)
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import Data.ByteString (ByteString)
import Prelude hiding (take, takeWhile)

data Result a = Fail String
              | OK !a

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype Parser a = Parser {
      runParser :: ForeignPtr Word8 -- payload
                -> Int              -- offset
                -> Int              -- length
                -> (# Result a, ForeignPtr Word8, Int, Int #)
    }

instance Functor Parser where
    fmap g m = Parser $ \f o l -> case runParser m f o l of
                (# OK a, f', o', l' #)     -> (# OK (g a), f', o', l' #)
                (# Fail err, f', o', l' #) -> (# Fail err, f', o', l' #)
    {-# INLINE fmap #-}

instance Monad Parser where
    return a = Parser $ \f o l -> (# OK a, f, o, l #)
    {-# INLINE return #-}

    m >>= k   = Parser $ \f o l -> case runParser m f o l  of
                 (# OK a, f', o', l' #) -> runParser (k a) f' o' l'
                 (# Fail err, f', o', l' #) -> (# Fail err, f', o', l' #)
    {-# INLINE (>>=) #-}

    fail msg = Parser $ \f o l -> (# Fail msg, f, o, l #)

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}

    mplus a b = Parser $ \f o l ->
                case runParser a f o l of
                  (# ok@OK{}, f', o', l' #) -> (# ok, f', o', l' #)
                  (# _, _, _, _ #) -> case runParser b f o l of
                                   (# ok@OK{}, f', o', l' #) -> (# ok, f', o', l' #)
                                   (# err, f', o', l' #) -> (# err, f', o', l' #)
    {-# INLINE mplus #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Applicative Parser where
    pure   = return
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p (PS fptr offset availSize) = case runParser p fptr offset availSize of
        (# OK a, _, _, _ #)     -> Right a
        (# Fail err, _, _, _ #) -> Left err
{-# INLINE parse #-}

{-# INLINE consumeWord8 #-}
consumeWord8 :: Parser Word8
consumeWord8 = Parser $ \f o l ->
    let w8 = inlinePerformIO $ withForeignPtr f $ \p -> peekByteOff p o
    in (# OK w8, f, o+1, l-1 #)

-- | Consume @n@ bytes of input.
take :: Int -> Parser ByteString
take !n = do
    !l <- getsLength
    if l >= n
        then unsafeTake n
        else fail "insufficient input for take"
{-# INLINE take #-}


getsPtr :: Parser (ForeignPtr Word8)
getsPtr = Parser $ \f o l -> (# OK f, f, o, l #)

getsOffset :: Parser Int
getsOffset = Parser $ \f o l -> (# OK o, f, o, l #)

getsLength :: Parser Int
getsLength = Parser $ \f o l -> (# OK l, f, o, l #)

ensureBytesLeft :: Int -> Parser ()
ensureBytesLeft ex = do
    l <- getsLength
    if ex == l
        then return ()
        else fail $ "Unexpected length: expected " ++ show ex ++
                ", but got " ++ show l
{-# INLINE ensureBytesLeft #-}

-- | Consume @n@ bytes of input without checking if it's available
unsafeTake :: Int -> Parser ByteString
unsafeTake !n = Parser $ \f o l ->
    let bs = PS f o n
    in (# OK bs, f, o + n, l - n #)
{-# INLINE unsafeTake #-}

-- | Skip @n@ bytes of input
skip :: Int -> Parser ()
skip !n = do
    l <- getsLength
    if l >= n
        then unsafeSkip n
        else fail "insufficient input for skip"
{-# INLINE skip #-}

-- | Skip @n@ bytes of input without checking if it's available
unsafeSkip :: Int -> Parser ()
unsafeSkip !n = Parser $ \f o l -> (# OK (), f, o + n, l - n #)
{-# INLINE unsafeSkip #-}


-- | Match a string exactly.
string :: ByteString -> Parser ()
string s@(PS _ _ l) = Parser $ \fptr offset avail ->
    if s == PS fptr offset (min l avail)
        then (# OK (), fptr, offset + l, avail - l #)
        else (# Fail ("string:"), fptr, offset, avail #)
{-# INLINE string #-}

anyChar :: Parser Char
anyChar = do
    s <- consumeWord8
    return (w2c $! s)
{-# INLINE anyChar #-}

w2c :: Word8 -> Char
w2c = toEnum . fromEnum
{-# INLINE w2c #-}
