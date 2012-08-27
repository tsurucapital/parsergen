-- | 'atoi' based parser
module Atoi
    ( unsafeDecimalX
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BI
import Foreign.C.Types (CChar, CLong (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)

import qualified ParserGen.Parser as P

foreign import ccall unsafe "hs_atoi" hs_atoi
    :: Ptr CChar -> Int -> IO CLong

atoi :: ByteString -> Int
atoi bs = unsafePerformIO $ withForeignPtr str_fptr $ \str_ptr -> do
    let start = str_ptr `plusPtr` offset
    x <- hs_atoi start len
    return $! fromIntegral x
 where
    (str_fptr, offset, len) = BI.toForeignPtr bs

unsafeDecimalX :: Int -> P.Parser Int
unsafeDecimalX len = do
    bs <- P.take len
    return $ atoi bs
