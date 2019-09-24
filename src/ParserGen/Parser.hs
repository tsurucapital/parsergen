-- A tiny, highly specialized combinator parser for 'B.ByteString'
-- strings. Designed to split bytestrings into fields with fixed widths.
--
-- unsafe versions of the functions do not perform checks that there
-- is enough data left in the bytestring
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module ParserGen.Parser
    ( Parser(..), gets
    , parse
    , ensureBytesLeft
    , ensureAtLeastBytesLeft
    , atEnd
    , string
    , anyChar, char
    , peekChar
    , peekBS
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Prelude hiding (take, takeWhile)

newtype S = S {
      input :: ByteString
    }

newtype Parser a = Parser { runParser :: forall r. S -> (S -> a -> r) -> (String -> r) -> r }

instance Functor Parser where
    {-# INLINE fmap #-}
    fmap f p = Parser $ \st good bad ->
        let onGood s a = s `seq` good s (f a)
            onBad  msg = bad msg
         in st `seq` runParser p st onGood onBad

instance Applicative Parser where
    pure v = Parser $ \st good _ -> st `seq` good st v
    (<*>) = ap

instance Monad Parser where
    return v = Parser $ \st good _ -> good st v
    a >>= b = Parser $ \st good bad ->
        let goodA !st' !a' = runParser (b a') st' good bad
         in st `seq` runParser a st goodA bad

instance MonadFail Parser where
    fail msg = Parser $ \_ _ bad -> bad msg

instance MonadPlus Parser where
    mzero = Parser $ \_ _ bad -> bad "mzero"
    mplus a b = Parser $ \st good bad ->
         st `seq` runParser a st good (\_ -> runParser b st good bad)

instance Alternative Parser where
    {-# INLINE empty #-}
    empty = mzero
    (<|>) = mplus


gets :: (S -> a) -> Parser a
gets f = Parser $ \st good _ -> good st (f st)
{-# INLINE gets #-}

put :: S -> Parser ()
put st = Parser $ \_ good bad -> good st ()
{-# INLINE put #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = runParser p (S bs) (const Right) Left
{-# INLINE parse #-}


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

ensureAtLeastBytesLeft :: Int -> Parser ()
ensureAtLeastBytesLeft l = do
    s <- gets input
    if B.length s >= l
        then return ()
        else fail $ "Unexpected length: expected at least " ++ show l ++
                ", but got " ++ show (B.length s)
{-# INLINE ensureAtLeastBytesLeft #-}

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
    else fail "string"
{-# INLINE string #-}

{-# INLINE anyChar #-}
anyChar :: Parser Char
anyChar = do
    s <- gets input
    put (S (B.unsafeDrop 1 s))
    return (w2c $! B.unsafeHead s)

{-# INLINE char #-}
char :: Char -> Parser ()
char c = do
    s <- gets input
    let c' = w2c $! B.unsafeHead s
    if c == c'
        then put (S (B.unsafeDrop 1 s))
        else fail "unexpected char"

{-# INLINE word8 #-}
word8 :: Word8 -> Parser ()
word8 w = do
    i <- gets input
    if B.unsafeHead i == w
        then put (S (B.unsafeDrop 1 i))
        else fail "word8"

{-# INLINE peekChar #-}
peekChar :: Parser Char
peekChar = do
    s <- gets input
    if B.null s
        then fail "no input"
        else return (w2c $! B.unsafeHead s)

{-# INLINE peekBS #-}
peekBS :: Parser ByteString
peekBS = gets input

-- | Indicate whether the end of the input has been reached.
atEnd :: Parser Bool
atEnd = do
  i <- gets input
  return $! B.null i
{-# INLINE atEnd #-}

w2c :: Word8 -> Char
w2c = toEnum . fromEnum
{-# INLINE w2c #-}
