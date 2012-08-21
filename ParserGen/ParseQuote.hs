{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module ParserGen.ParseQuote
    ( Decl (..)

    , Datatype (..)
    , DataConstructor (..)
    , DataField (..)
    , ParserType (..)

    , Repacker (..)
    , RepackerField (..)

    , getDecls
    , unzipDecls
    , getDatatypes
    , getRepackers

    , getFieldWidth
    , getConstructorWidth
    ) where


--import Control.Monad.Trans.Class
import Control.Monad (unless)
import Data.List (isPrefixOf)
import Text.Parsec hiding (spaces)
import Text.Parsec.Pos
import Language.Haskell.TH as TH
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>), takeDirectory)

import Data.Char (chr)
import Control.Applicative hiding (many, (<|>), optional)

import Debug.Trace

data Decl
    = DatatypeDecl Datatype
    | RepackerDecl Repacker
    deriving (Show)

data Datatype
    = Datatype
    { typeName     :: String
    , typeConstrs  :: [DataConstructor]
    } deriving (Show)

data DataConstructor
    = DataConstructor
    { constrName   :: String
    , constrPrefix :: Maybe String
    , constrFields :: [DataField]
    } deriving (Show)

data DataField
    = DataField
    { fieldName    :: Maybe String
    , fieldRepeat  :: Maybe Int
    , fieldType    :: Type
    , fieldStrict  :: Bool
    , fieldWidth   :: Int
    , fieldParser  :: ParserType
    } deriving (Show)

data ParserType
    = CustomParser    Exp    -- user provided parser, ex: issue
    | UnsignedParser         -- type/newtype wrapper around supported datatypes
    | SignedParser           -- type/newtype wrapper around numerical datatypes only
    | HardcodedString String -- raw string, ex "B7014"
    deriving (Show, Eq)

data Repacker = Repacker
    { repackerName        :: String
    , repackerConstructor :: String
    , repackerFields      :: [RepackerField]
    } deriving (Show)

data RepackerField = RepackerField
    { repackerFieldName     :: String
    , repackerFieldUnparser :: Exp
    } deriving (Show)

-- get size to skip taking into account its repetition and sign if exists
getFieldWidth :: DataField -> Int
getFieldWidth (DataField {..}) =
    let width = fieldWidth + if fieldParser == SignedParser then 1 else 0
        times = maybe 1 id fieldRepeat
    in width * times

getConstructorWidth :: DataConstructor -> Int
getConstructorWidth = sum . map getFieldWidth . constrFields

type ParserQ = ParsecT String () Q

getDecls :: FilePath -> Q [Decl]
getDecls templateName = do
    tpl  <- getTemplate templateName
    dcls <- parseDecls tpl
    traceShow dcls $ return dcls

unzipDecls :: [Decl] -> ([Datatype], [Repacker])
unzipDecls decls =
    ( [d | DatatypeDecl d <- decls]
    , [r | RepackerDecl r <- decls]
    )

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

getQPos :: Q SourcePos
getQPos = do
    loc <- TH.location
    return $ newPos (TH.loc_filename loc)
                    (fst . TH.loc_start $ loc)
                    (snd . TH.loc_start $ loc)

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
        _ <- try (string "    ") <?> "field padding"

        fieldRepeat  <- optionMaybe (try $ repeatFactor <* spaces)

        fieldName    <- (Just <$> identifier) <|> (Nothing <$ (char '_' <* identifier))

        _ <- spaces

        fieldStrict  <- try (char '!' *> return True <* spaces) <|> return False

        fieldType    <- typeParser


        _ <- spaces

        (signed, fieldWidth) <- widthSpec


        if signed
            then do let fieldParser = SignedParser
                    _ <- endofline <?> "signed only parser"
                    return DataField {..}
            else do
                    fieldParser <- choice [ CustomParser    <$> try (spaces *> customParser)
                                          , HardcodedString <$> try (spaces *> hardcodedString)
                                          , return UnsignedParser
                                          ]
                    _ <- endofline

                    return DataField {..}


widthSpec :: ParserQ (Bool, Int)
widthSpec = ((,) <$> (option False (True <$ char '+')) <*> decimal) <?> "field width spec"


typeParser :: ParserQ Type
typeParser = (singleWord <|> multiWord) <?> "field type"
    where
        singleWord = (TH.ConT . TH.mkName) <$> ((:) <$> letter <*> many alphaNum)
        multiWord = error "multiWord is not yet implemented"
--        multiWord = between (char '(') (char ')') (many1 (noneOf ")"))


customParser :: ParserQ Exp
customParser = singleWord <?> "custom parser"
    where
        singleWord = (TH.VarE . TH.mkName) <$> ((:) <$> lower <*> many1 (noneOf "( )\t\n"))

hardcodedString :: ParserQ String
hardcodedString = between (char '"') (char '"') (many1 $ escapedChar <|> notQuote) <?> "hardcoded string"
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
    <*  spaces
    <*> customParser
    <*  endofline

decimal :: ParserQ Int
decimal = read <$> many1 digit

identifier :: ParserQ String
identifier = ((:) <$> upper <*> many alphaNum)

prefix :: ParserQ String
prefix = ((:) <$> lower <*> many alphaNum)

endofline :: ParserQ [Char]
endofline = many1 (try $ many (oneOf "\t ") *> (option "" $ try comment) *> char '\n') <?> "end of line"
    where
        comment = string "--" *> many1 (noneOf "\n")
