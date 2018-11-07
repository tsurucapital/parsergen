{-# LANGUAGE RecordWildCards #-}
module ParserGen.Types
    ( Decl (..)
    , unzipDecls

    , Datatype (..)

    , DataConstructor (..)
    , getConstructorWidth

    , DataField (..)
    , getFieldWidth
    , getFieldRepeatType
    , getFieldHasRepeat
    , getFieldIsIgnored, getFieldResultIsIgnored

    , ParserType (..)

    , Repacker (..)
    , RepackerField (..)
    ) where

import Data.Maybe (isJust, isNothing)
import Language.Haskell.TH (Exp, Type (..))

data Decl
    = DatatypeDecl Datatype
    | RepackerDecl Repacker
    deriving (Show)

unzipDecls :: [Decl] -> ([Datatype], [Repacker])
unzipDecls decls =
    ( [d | DatatypeDecl d <- decls]
    , [r | RepackerDecl r <- decls]
    )

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
    , constrMore   :: Bool
    } deriving (Show)

getConstructorWidth :: DataConstructor -> Int
getConstructorWidth = sum . map getFieldWidth . constrFields

data DataField
    = DataField
    { fieldName    :: Maybe String
    , fieldRepeat  :: Maybe Int
    , fieldType    :: Type
    , fieldStrict  :: Bool
    , fieldWidth   :: Int
    , fieldParser  :: ParserType
    } deriving (Show)

-- get size to skip taking into account its repetition and sign if exists
getFieldWidth :: DataField -> Int
getFieldWidth (DataField {..}) =
    let width = fieldWidth + if fieldParser == SignedParser then 1 else 0
        times = maybe 1 id fieldRepeat
    in width * times

getFieldRepeatType :: DataField -> Type
getFieldRepeatType df
    | getFieldHasRepeat df = AppT ListT $ fieldType df
    | otherwise            = fieldType df

getFieldHasRepeat :: DataField -> Bool
getFieldHasRepeat = isJust . fieldRepeat

getFieldIsIgnored :: DataField -> Bool
getFieldIsIgnored df = case fieldParser df of
    CustomParser    _ -> False
    HardcodedString _ -> False
    _                 -> getFieldResultIsIgnored df

getFieldResultIsIgnored :: DataField -> Bool
getFieldResultIsIgnored = isNothing . fieldName


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
    , repackerFieldUnparser :: Maybe Exp
    } deriving (Show)
