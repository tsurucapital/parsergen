{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module ParserGen.ParseQuote
    ( getDecls
    , getDatatypes
    , getRepackers
    ) where

import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad (unless, (>=>))
import Data.Char (chr)
import Data.List (isPrefixOf)
import Language.Haskell.TH as TH
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>), takeDirectory)
import Text.Parsec hiding (spaces)
import Text.Parsec.Pos

import ParserGen.Types

type ParserQ = ParsecT String () Q

getDecls :: FilePath -> Q [Decl]
getDecls = getTemplate >=> parseDecls

getDatatypes :: FilePath -> Q [Datatype]
getDatatypes = fmap (fst . unzipDecls) . getDecls

getRepackers :: FilePath -> Q [Repacker]
getRepackers = fmap (snd . unzipDecls) . getDecls

getTemplate :: FilePath -> Q (SourcePos, String)
getTemplate templateName = do
    filename <- loc_filename <$> location
    pwd <- runIO $ getCurrentDirectory
    let templatePath = (takeDirectory $ pwd </> filename) </> templateName
    body <- runIO $ readFile templatePath
    return (newPos templateName 1 1, body)

parseInQ :: ParserQ v -> (SourcePos, String) -> Q v
parseInQ p (pos, s) = do
        parseResult <- runParserT (inPosition p) () "" s
        case parseResult of
            Right v  -> return v
            Left err -> fail $ show err
    where
        inPosition :: ParserQ v -> ParserQ v
        inPosition p' = do
                setPosition pos
                val <- p'
                eof
                return val

parseDecls :: (SourcePos, String) -> Q [Decl]
parseDecls = parseInQ $ many1 $
    (DatatypeDecl <$> datatypeParser) <|>
    (RepackerDecl <$> repackerParser)

datatypeParser :: ParserQ Datatype
datatypeParser = do
    _           <- optional endofline
    typeName    <- identifier
    _           <- endofline
    typeConstrs <- many1 constrParser
    _           <- many endofline

    return Datatype {..}

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany1 (oneOf "\t ")

constrParser :: ParserQ DataConstructor
constrParser = do
    _            <- try (string "  " <?> "constructor padding")
    constrName   <- identifier
    constrPrefix <- optionMaybe (try $ spaces *> prefix)
    _            <- endofline
    constrFields <- many1 constFieldParser

    return DataConstructor {..}


repeatFactor :: ParserQ Int
repeatFactor = try (decimal <* char 'x') <?> "repetition factor"

constFieldParser :: ParserQ DataField
constFieldParser = do
    _           <- try (string "    ") <?> "field padding"
    fieldRepeat <- optionMaybe (try $ repeatFactor <* spaces)
    fieldName   <- fieldNameParser
    _           <- spaces
    fieldStrict <- try (char '!' *> return True <* spaces) <|> return False
    fieldType   <- typeParser
    _           <- spaces
    signed      <- option False (True <$ char '+')
    fieldWidth  <- decimal <?> "field width spec"
    fieldParser <- fieldParserParser signed
    _           <- endofline
    return DataField {..}

fieldNameParser :: ParserQ (Maybe String)
fieldNameParser =
    (Just <$> identifier) <|> (Nothing <$ (char '_' <* identifier))

typeParser :: ParserQ Type
typeParser = (singleWord <|> multiWord) <?> "field type"
  where
    singleWord = (TH.ConT . TH.mkName) <$> ((:) <$> letter <*> many alphaNum)
    multiWord  = error "multiWord is not yet implemented"
--        multiWord = between (char '(') (char ')') (many1 (noneOf ")"))


fieldParserParser :: Bool -> ParserQ ParserType
fieldParserParser signed =
    (if signed then pure SignedParser else fail "signed parser") <|>
    (CustomParser    <$> try (spaces *> customParser))           <|>
    (HardcodedString <$> try (spaces *> hardcodedString))        <|>
    (pure UnsignedParser)

customParser :: ParserQ Exp
customParser = singleWord <?> "custom parser"
  where
    singleWord = (TH.VarE . TH.mkName) <$>
        ((:) <$> lower <*> many1 (noneOf "( )\t\n"))

hardcodedString :: ParserQ String
hardcodedString =
    between (char '"') (char '"') (many1 $ escapedChar <|> notQuote) <?>
    "hardcoded string"
  where
    escapedChar = char '\\' *> (special <|> hex <|> dec)

    special :: ParserQ Char
    special = do
            c <- oneOf "nt\"\\"
            return $ case c of
                'n' -> '\n'
                't' -> '\t'
                v   -> v -- unescape for \" and \\

    hex :: ParserQ Char
    hex = char 'x' *> ((chr . read  . ("0x"++)) <$> many1 hexDigit)

    dec :: ParserQ Char
    dec = chr <$> decimal

    notQuote = noneOf ['"']

repackerParser :: ParserQ Repacker
repackerParser = Repacker
    <$> parseRepackerName
    <*  spaces
    <*> identifier
    <*  endofline
    <*> many1 parseRepackerField
    <*  many endofline

parseRepackerName :: ParserQ String
parseRepackerName = do
    name <- many1 alphaNum 
    unless ("repackerFor" `isPrefixOf` name) $ fail $
        "Repacker name must start with \"repackerFor\": " ++ name
    return name

parseRepackerField :: ParserQ RepackerField
parseRepackerField = RepackerField
    <$  (try (string "  ") <?> "repacker field padding")
    <*> identifier
    <*> optionMaybe (spaces *> customParser)
    <*  endofline

decimal :: ParserQ Int
decimal = read <$> many1 digit

identifier :: ParserQ String
identifier = ((:) <$> upper <*> many alphaNum)

prefix :: ParserQ String
prefix = ((:) <$> lower <*> many alphaNum)

endofline :: ParserQ [Char]
endofline = many1
    (try $ many (oneOf "\t ")  *> (option "" $ try comment) *> char '\n') <?>
    "end of line"
  where
    comment = string "--" *> many1 (noneOf "\n")
