{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Gen
    ( genDataTypeFromFile
    , genParserFromFile
    , genWidthFromFile
    ) where

import Language.Haskell.TH as TH
import Control.Applicative
import Control.Monad
import Data.Char (isUpper, toLower)
import Data.Maybe (catMaybes)

import ParserGen.Auto
import ParserGen.ParseQuote
import qualified ParserGen.Parser as P
import ParserGen.Types

genDataTypeFromFile :: FilePath -> Q [Dec]
genDataTypeFromFile templateName = getDatatypes templateName >>= mapM mkDataDecl

genParserFromFile :: FilePath -> Q [Dec]
genParserFromFile = getDatatypes >=> fmap concat . mapM mkParsersDecls

genWidthFromFile :: FilePath -> Q [Dec]
genWidthFromFile = getDatatypes >=> fmap concat . mapM mkWidthDecls

mkDataDecl :: Datatype -> Q Dec
mkDataDecl (Datatype {..}) = do
    constrs <- mapM mkConstDef typeConstrs
    return $ DataD [] (mkName typeName) [] constrs [''Eq, ''Show]
  where
    mkConstDef :: DataConstructor -> Q Con
    mkConstDef dc@(DataConstructor {..}) = do
        fields <- catMaybes <$> mapM (mkFieldDef dc) constrFields
        return $ RecC (mkName constrName) fields


mkFieldDef :: DataConstructor -> DataField -> Q (Maybe (Name, Strict, Type))
mkFieldDef dc@(DataConstructor {..}) df@(DataField {..}) = return $ do
    name <- getFieldName dc df
    return (name, strict, getFieldRepeatType df)
  where
    strict :: Strict
    strict = if fieldStrict then IsStrict else NotStrict

getFieldName :: DataConstructor -> DataField -> Maybe Name
getFieldName (DataConstructor {..}) (DataField {..}) =
    mkName <$> ((++) <$> (constrPrefix <|> defaultPrefix) <*> fieldName)
  where
    defaultPrefix = Just (map toLower . filter isUpper $ constrName)


-- to create separate parsers for each constructor
mkParsersDecls :: Datatype -> Q [Dec]
mkParsersDecls (Datatype {..}) =
    concat <$> mapM (mkConstrParser typeName) typeConstrs
  where
    mkConstrParser :: String -> DataConstructor -> Q [Dec]
    mkConstrParser name dc@(DataConstructor {..}) = do
        fields <- mapM mkField (fuseIgnores constrFields)
        ensure <- ensureBytes $ getConstructorWidth dc
        t      <- [t| P.Parser |]
        return
            [ SigD funName (AppT t (ConT . mkName $ name ))
            , FunD funName
                [Clause [] (NormalB . DoE $ ensure : fields ++ [result] ) []]
            ]
      where

        ensureBytes :: Int -> Q Stmt
        ensureBytes t = [| P.ensureBytesLeft t |] >>= return . NoBindS

        funName :: Name
        funName = mkName $ "parserFor" ++ constrName

        mkField :: DataField -> Q Stmt
        mkField df@(DataField {..}) = do
            (parser, _) <- getFieldParserUnparser df Nothing
            return $ case getFieldName dc df of
                Just n -> BindS (VarP n) parser
                _      -> NoBindS parser

        result :: Stmt
        result = NoBindS (AppE (VarE . mkName $ "return")
             (RecConE (mkName constrName)
                      (concatMap mkFieldAssignment constrFields)))

        mkFieldAssignment :: DataField -> [FieldExp]
        mkFieldAssignment df@(DataField {..}) = case getFieldName dc df of
                Just n  -> [(n, VarE n)]
                Nothing -> []

-- | Transforms sequence of size-based parsers with ignored values into one
-- larger parser
fuseIgnores :: [DataField] -> [DataField]
fuseIgnores (a : b : rest)
    | getFieldIsIgnored a && getFieldIsIgnored b = fuseIgnores $ fused : rest
    | otherwise                                  = a : fuseIgnores (b : rest)
  where
    fused = DataField { fieldName   = Nothing
                      , fieldStrict = False
                      , fieldRepeat = Nothing
                      , fieldType   = (ConT . mkName $ "()")
                      , fieldParser = UnsignedParser
                      , fieldWidth  = getFieldWidth a + getFieldWidth b
                      }

fuseIgnores (x : xs) = x : fuseIgnores xs
fuseIgnores []       = []

mkWidthDecls :: Datatype -> Q [Dec]
mkWidthDecls (Datatype {..}) = concat <$> mapM mkConstrWidthDecl typeConstrs
  where
    mkConstrWidthDecl :: DataConstructor -> Q [Dec]
    mkConstrWidthDecl dc@(DataConstructor {..}) = return
        [ SigD name (ConT $ mkName "Int")
        , FunD name [Clause [] (NormalB $ LitE $ IntegerL width) []]
        ]
      where
        width = fromIntegral $ getConstructorWidth dc
        name  = mkName $ "widthFor" ++ constrName
