module ParserGen.Common.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements)

import ParserGen.Common
import ParserGen.Parser

tests :: Test
tests = testGroup "ParserGen.Common.Tests"
    [ testProperty "decimalX"  testDecimalX
    , testProperty "decimalXS" testDecimalXS
    , testProperty "alphaNum"  testAlphaNum
    ]

newtype DecimalX = DecimalX Int
    deriving (Eq, Show)

instance Arbitrary DecimalX where
    arbitrary = DecimalX . (flip mod 1000000) . abs <$> arbitrary

testDecimalX :: DecimalX -> Bool
testDecimalX (DecimalX x) =
    parse (unsafeDecimalX 6) (B.concat $ putDecimalX 6 x) == Right x

newtype DecimalXS = DecimalXS Int
    deriving (Eq, Show)

instance Arbitrary DecimalXS where
    arbitrary = do
        DecimalX x <- arbitrary
        neg        <- arbitrary
        return $ DecimalXS $ if neg then negate x else x

testDecimalXS :: DecimalXS -> Bool
testDecimalXS (DecimalXS x) =
    parse (unsafeDecimalXS 6) (B.concat $ putDecimalXS 6 x) == Right x

newtype AlphaNumTest = AlphaNumTest ByteString
    deriving (Eq, Show)

instance Arbitrary AlphaNumTest where
    arbitrary = AlphaNumTest . BC.pack <$> replicateM 12 (elements chars)
      where
        chars = (['A' .. 'Z'] ++ ['0' .. '9'])

testAlphaNum :: AlphaNumTest -> Bool
testAlphaNum (AlphaNumTest bs) =
    either error putAlphaNum (parse (unsafeAlphaNum 12) bs) == bs
