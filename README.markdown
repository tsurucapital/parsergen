parsergen
=========

[![Build Status](https://secure.travis-ci.org/tsurucapital/parsergen.png?branch=master)](http://travis-ci.org/tsurucapital/parsergen)

Introduction
------------

`parsergen` is a library aimed at generating fast Haskell parsers for fixed
width packets. It uses a DSL in which these packets can be specified, augmented
with Haskell parsers.

In order to create a packet and a parser for it, usually two files are used,
`Foo.hs` and `Foo.ths`.

Tutorial
--------

### Datatypes and parsers

#### Syntax

Let's start by defining a datatype in the `.ths` file. The syntax here is:

    TypeName
      ConstructorName [fields prefix]
        [Nx] [_]FieldName [!] FieldType [+]FieldWidth [FieldParser]

where

- `TypeName`: Name of the type itself, e.g. `Maybe`

- `ConstructorName`: Name of constructor with given set of fields. If no prefix
  is provided, downcased capital letters from the constructor name will be used
  instead.

- `Nx`: Number of times to repeat this matcher

- `FieldName`: Name of the field which will be used (with constructor prefix
  prepended)

- `_`: This field will be ignored (skipped if possible or parsed)

- `!`: This field will be strict

- `FieldType`: type name when using existing datatype, e.g. `Int` or
  `ByteString`, or a custom type `Foo`

- `FieldWidth`: Number for size based parsing, e.g. `12`. This field is needed
  to perform some optimisations as well, so you have to specify field width even
  if you going to specify `FieldParser`.

- `+`: Only for numerical fields: the first character will be treated as the
  sign

- `FieldParser`: A parser which will be used to parse it. This can be omitted
  for types such as `Int` or `ByteString`. Otherwise, you can either specify a
  fixed string or a parser of the type `Parser`.

In the `.hs` file, one can now use:

    $(genDataTypeFromFile "Foo.ths")
    $(genParserFromFile   "Foo.ths")

to generate a parser and a datatype for it.

#### Example

Let's look at an example `.ths` file:

    Packet
      Warning
        _PacketType       ByteString     4  "WARN"
        DangerType        DangerType     2  dangerType
        ChanceOfSurvival  Int            3

      LotteryWin
        _PacketType       ByteString     4  "LOTT"
        Amount            Money         10
        6x WinningEntry   LotteryEntry   2

And the `.hs` file:

    {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
    import Data.ByteString (ByteString)
    import ParserGen.Gen
    import ParserGen.Repack  -- Needed later on
    import qualified ParserGen.Parser as P

    data DangerType
        = Earthquake
        | ZombieApocalypse
        | RobotUprising
        | AngryGirlfriend
        deriving (Eq, Show)

    dangerType :: P.Parser DangerType
    dangerType = do
        bs <- P.take 2
        case bs of
            "EQ" -> return Earthquake
            "ZA" -> return ZombieApocalypse
            "RI" -> return RobotUprising
            "AG" -> return AngryGirlfriend
            _    -> fail $ "Unknown danger type: " ++ show bs

    newtype Money = Money Int
        deriving (Eq, Show)

    type LotteryEntry = Int

    $(genDataTypeFromFile "Packet.ths")
    $(genParserFromFile   "Packet.ths")

    sampleWarning :: ByteString
    sampleWarning = "WARNRI002"

    sampleLotteryWin :: ByteString
    sampleLotteryWin = "LOTT9999999999040815162342"

    main :: IO ()
    main = do
        print $ P.parse parserForWarning sampleWarning
        print $ P.parse parserForLotteryWin sampleLotteryWin

The `parsergen` generates:

- The `Packet` datatype
- The parser functions `parserForWarning, parserForLotteryWin :: Parser Packet`

Note how we have used three kinds of parsers:

- `"WARN"` is an example of a hardcoded string which the packet must match
- `dangerType` is a custom parser, specified in the Haskell file
- We don't specify parsers for numeral types, these are automatically derived
  (even for `newtype`s and `type` synonyms)

### Repackers

A powerful feature from the library, repackers allow us to change the contents
of multiple fields without actually parsing a packet.

#### Syntax

The syntax looks like this:

    repackerForName ConstructorName
      FieldName [FieldUnParser]

#### Example

Let's add the following the bottom of our `.ths` file:

    repackerForLotteryNumbers LotteryWin
      WinningEntry

And the following to our Haskell file:

    $(genRepackFromFile "Packet.ths")

which generates the function

    repackerForLotteryNumbers :: [LotteryEntry] -> ByteString -> ByteString

Use it like:

    print $ repackerForLotteryNumbers [1 .. 6] sampleLotteryWin

Things to note:

- For numerical types, you don't need to specify an unparser, this is only
  needed for custom types. These should have the type `SomeType -> ByteString`.
- The repacker will take a list when the field is repeated (e.g. `6x` in this
  case) and a single value otherwise
