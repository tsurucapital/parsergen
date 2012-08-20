-- | This library provides a simple DSL for defining fixed-width datatypes, and
-- Template Haskell generators for creating fast parsers.
--
-- /SYNTAX/
--
-- A @.ths@ file is used to specify datatypes and looks like:
--
-- > TypeName
-- >   ConstructorName [fields prefix]
-- >     [Nx] [_]FieldName [!] FieldType [+]FieldWidth [FieldParser]
--
--
-- where
--
-- [@TypeName@] Name of the type itself, e.g. 'Maybe'
--
-- [@ConstructorName@] Name of constructor with given set of fields. If no
-- prefix is provided, downcased capital letters from the constructor name will
-- be used instead.
--
-- [@Nx@] Number of times to repeat this matcher
--
-- [@FieldName@] Name of the field which will be used (with constructor prefix
-- prepended)
--
-- [@_@] This field will be ignored (skipped if possible or parsed)
--
-- [@!@] This field will be strict
--
-- [@FieldType@] type name when using existing datatype, e.g. 'Int' or
-- 'ByteString', or a custom type @Foo@
--
-- [@FieldWidth@] Number for size based parsing, e.g. @12@. This field is needed
-- to perform some optimisations as well, so you have to specify field width
-- even if you going to specify @FieldParser@.
--
-- [@+@] Only for numerical fields: the first character will be treated as the
-- sign
--
-- [@FieldParser@] A parser which will be used to parse it. This can be omitted
-- for types such as 'Int' or 'ByteString'. Otherwise, you can either specify a
-- fixed string or a parser of the type 'Parser'.
--
-- /EXAMPLE/
--
-- @Foo.ths@:
--
-- > Foo
-- >   Bar
-- >     _Prefix      ByteString  3  "BAR"
-- >     3x MyNumber  MyNumber    4
-- >     Sauce        Sauce       3  sauce
-- >
-- >   Qux
-- >     _Prefix ByteString 3 "QUX"
-- >     Name    ByteString 5
--
-- @Foo.hs@:
--
-- > {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- > import Control.Applicative ((<|>))
-- > import Data.ByteString (ByteString)
-- >
-- > import ParserGen.Gen
-- > import qualified ParserGen.Parser as P
-- >
-- > newtype MyNumber = MyNumber Int
-- >     deriving (Eq, Show)
-- >
-- > data Sauce = Barbeque | Garlic | Ketchup
-- >     deriving (Eq, Show)
-- >
-- > sauce :: P.Parser Sauce
-- > sauce = do
-- >     code <- P.take 3
-- >     case code of
-- >         "BBQ" -> return Barbeque
-- >         "GLC" -> return Garlic
-- >         "KCP" -> return Ketchup
-- >         _     -> fail $ "Unknown sauce code: " ++ show code
-- >
-- > $(genDataTypeFromFile "Foo.ths")
-- > $(genParserFromFile   "Foo.ths")
-- >
-- > main :: IO ()
-- > main = print $ P.parse (parserForBar <|> parserForQux)
-- >     "BAR321849239123BBQ"
--
-- This prints:
--
-- > Right (Bar
-- >     { bMyNumber = [MyNumber 3218, MyNumber 4923, MyNumber 9123]
-- >     , bSauce = Barbeque
-- >     })
--
module ParserGen
    ( module ParserGen.Gen
    , module ParserGen.ParseQuote
    , module ParserGen.Parser
    , module ParserGen.Repack
    , module ParserGen.Wrap
    ) where

import ParserGen.Gen
import ParserGen.ParseQuote
import ParserGen.Parser
import ParserGen.Repack
import ParserGen.Wrap
