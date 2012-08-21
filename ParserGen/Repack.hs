{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Repack
    ( RepackConstructor
    , repackConstructor

    , RepackField
    , repackField

    , genRepackFromFile
    ) where

import Control.Applicative
import Control.Monad (foldM, guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Lift)
import Unsafe.Coerce (unsafeCoerce)

import ParserGen.ParseQuote

import Debug.Trace

data RepackConstructor = RepackConstructor
    { repackConstrName   :: String
    , repackConstrSuffix :: String
    , repackConstrFields :: [RepackField]
    } deriving (Show)

repackConstructor :: String -> String -> [RepackField] -> RepackConstructor
repackConstructor = RepackConstructor

data RepackField = RepackField
    { repackFieldName      :: String
    , repackFieldTransform :: ExpQ
    }

instance Show RepackField where
    show (RepackField name _) = "(RepackField " ++ show name ++ " _)"

repackField :: String -> ExpQ -> RepackField
repackField name exp = RepackField name exp

class Repackable a where
    repack :: a -> [ByteString]  -- TODO blaze builder

instance Repackable a => Repackable [a] where
    repack = concat . map repack
    {-# INLINE repack #-}

instance Repackable ByteString where
    repack = return
    {-# INLINE repack #-}

genRepackFromFile :: FilePath -> [RepackConstructor] -> Q [Dec]
genRepackFromFile templateName rcs = do
    dts <- getDatatypes templateName
    fmap concat $ mapM (mkRepacker dts) rcs

mkRepacker :: [Datatype] -> RepackConstructor -> Q [Dec]
mkRepacker dts (RepackConstructor cname csuffix cfields) = do
    withNames <- mapM (\cf -> (,) cf <$> newName "p") cfields
    let repackCmds = mkRepackCmds dc withNames

    bsVar    <- newName "bs"
    undef    <- [|undefined|]
    btbT     <- [t|ByteString -> ByteString|]
    foldInit <- [|($(return $ VarE bsVar) , [])|]
    fold     <- foldM executeRepackCmd foldInit repackCmds
    body     <- [|B.concat $ snd $(return fold)|]

    return
        [ SigD repackerName (foldr mkType btbT cfields)
        , FunD repackerName
            [ Clause (map (VarP . snd) withNames ++ [VarP bsVar])
              (NormalB body) []
            ]
        ]
  where
    repackerName = mkName $ "repackerFor" ++ cname ++ csuffix

    dc = case [c | dt <- dts, c <- typeConstrs dt, constrName c == cname] of
        [x] -> x
        _   -> error $ "No genparser for constructor " ++ cname

    mkType (RepackField name _) t =
        AppT (AppT ArrowT (getFieldType name dc)) t

getFieldType :: String -> DataConstructor -> Type
getFieldType n dc =
    case [fieldType f | f <- constrFields dc, fieldName f == Just n] of
        [t] -> t
        _   -> error $ constrName dc ++ " has no field " ++ n

data RepackCmd
    = Skip Int
    | Repack Int RepackField Name
    deriving (Show)

fuseSkips :: [RepackCmd] -> [RepackCmd]
fuseSkips (Skip a : Skip b : rcs) = fuseSkips $ Skip (a + b) : rcs
fuseSkips (r      : rcs)          = r : fuseSkips rcs
fuseSkips []                      = []

mkRepackCmds :: DataConstructor -> [(RepackField, Name)] -> [RepackCmd]
mkRepackCmds dc repacks = fuseSkips $ map mkRepackCmd $ constrFields dc
  where
    mkRepackCmd :: DataField -> RepackCmd
    mkRepackCmd df = fromMaybe (Skip $ getFieldWidth df) $ listToMaybe
        [ Repack (getFieldWidth df) rf n
        | (rf, n) <- repacks
        , fieldName df == Just (repackFieldName rf)
        ]

executeRepackCmd :: Exp -> RepackCmd -> Q Exp
executeRepackCmd exp (Skip n) =
    [| let (s, ps)      = $(return exp)
           (this, next) = B.splitAt n s
       in (next, ps ++ [this]) |]
executeRepackCmd exp (Repack n (RepackField _ f) name) =
    [| let (s, ps)      = $(return exp)
           (this, next) = B.splitAt n s
       in (next, ps ++ repack ($(f) $(return $ VarE name))) |]
