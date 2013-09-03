-- | Based on Data.Attoparsec.Zepto by  Bryan O'Sullivan 2011
--
-- A tiny, highly specialized combinator parser for 'B.ByteString'
-- strings. Designed to split bytestrings into fields with fixed widths.
--
-- unsafe versions of the functions do not perform checks that there
-- is enough data left in the bytestring
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module ParserGen.Parser
    ( Parser
    , parse
    , ensureBytesLeft
    , atEnd
    , string
    , anyChar
    , word8
    , take
    , unsafeTake
    , skip
    , unsafeSkip
    , takeWhile
    ) where

import Data.Word (Word8)
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Prelude hiding (take, takeWhile)

newtype S = S {
      input :: ByteString
    }

data Result a = Fail String
              | OK !a

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype Parser a = Parser {
      runParser :: S -> (# Result a, S #)
    }

instance Functor Parser where
    fmap f m = Parser $ \s -> case runParser m s of
                                (# OK a, s' #)     -> (# OK (f a), s' #)
                                (# Fail err, s' #) -> (# Fail err, s' #)
    {-# INLINE fmap #-}

instance Monad Parser where
    return a = Parser $ \s -> (# OK a, s #)
    {-# INLINE return #-}

    m >>= k   = Parser $ \s -> case runParser m s of
                                 (# OK a, s' #) -> runParser (k a) s'
                                 (# Fail err, s' #) -> (# Fail err, s' #)
    {-# INLINE (>>=) #-}

    fail msg = Parser $ \s -> (# Fail msg, s #)

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}

    mplus a b = Parser $ \s ->
                case runParser a s of
                  (# ok@(OK _), s' #) -> (# ok, s' #)
                  (# _, _ #) -> case runParser b s of
                                   (# ok@(OK _), s'' #) -> (# ok, s'' #)
                                   (# err, s'' #) -> (# err, s'' #)
    {-# INLINE mplus #-}

instance Applicative Parser where
    pure   = return
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

gets :: (S -> a) -> Parser a
gets f = Parser $ \s -> (# OK (f s), s #)
{-# INLINE gets #-}

put :: S -> Parser ()
put s = Parser $ \_ -> (# OK (), s #)
{-# INLINE put #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = case runParser p (S bs) of
    (# OK a, _ #)     -> Right a
    (# Fail err, _ #) -> Left err
{-# INLINE parse #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

-- | Consume input while the predicate returns 'True'.
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile p = do
  (h,t) <- gets (B.span p . input)
  put (S t)
  return h
{-# INLINE takeWhile #-}

-- | Consume @n@ bytes of input.
take :: Int -> Parser ByteString
take !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s)) >> return (B.unsafeTake n s)
    else fail "insufficient input for take"
{-# INLINE take #-}

ensureBytesLeft :: Int -> Parser ()
ensureBytesLeft l = do
    s <- gets input
    if B.length s == l
        then return ()
        else fail $ "Unexpected length: expected " ++ show l ++
                ", but got " ++ show (B.length s)
{-# INLINE ensureBytesLeft #-}

-- | Consume @n@ bytes of input without checking if it's available
unsafeTake :: Int -> Parser ByteString
unsafeTake !n = do
    s <- gets input
    put (S (B.unsafeDrop n s))
    return (B.unsafeTake n s)
{-# INLINE unsafeTake #-}

-- | Skip @n@ bytes of input
skip :: Int -> Parser ()
skip !n = do
    s <- gets input
    if B.length s >= n
        then put (S (B.unsafeDrop n s))
        else fail "insufficient input for skip"
{-# INLINE skip #-}

-- | Skip @n@ bytes of input without checking if it's available
unsafeSkip :: Int -> Parser ()
unsafeSkip !n = gets input >>= put . S . B.unsafeDrop n
{-# INLINE unsafeSkip #-}

-- | Match a string exactly.
string :: ByteString -> Parser ()
string s = do
  i <- gets input
  if s `B.isPrefixOf` i
    then put (S (B.unsafeDrop (B.length s) i))
    else fail $ "string"
{-# INLINE string #-}

anyChar :: Parser Char
anyChar = do
    s <- gets input
    put (S (B.unsafeDrop 1 s))
    return (w2c $! B.unsafeHead s)

word8 :: Word8 -> Parser ()
word8 w = do
    i <- gets input
    if B.unsafeHead i == w
        then put (S (B.unsafeDrop 1 i))
        else fail "word8"

-- | Indicate whether the end of the input has been reached.
atEnd :: Parser Bool
atEnd = do
  i <- gets input
  return $! B.null i
{-# INLINE atEnd #-}

w2c :: Word8 -> Char
w2c = toEnum . fromEnum
{-# INLINE w2c #-}
