{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Repack
    ( genRepackFromFile
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

genRepackFromFile :: FilePath -> Q [Dec]
genRepackFromFile templateName = do
    (dts, rs) <- unzipDecls <$> getDecls templateName
    fmap concat $ mapM (mkRepacker dts) rs

mkRepacker :: [Datatype] -> Repacker -> Q [Dec]
mkRepacker dts (Repacker rname cname cfields) = do
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
    repackerName = mkName rname

    dc = case [c | dt <- dts, c <- typeConstrs dt, constrName c == cname] of
        [x] -> x
        _   -> error $ "No genparser for constructor " ++ cname

    mkType (RepackerField name _) t =
        AppT (AppT ArrowT (getFieldType name dc)) t

getFieldType :: String -> DataConstructor -> Type
getFieldType n dc =
    case [fieldType f | f <- constrFields dc, fieldName f == Just n] of
        [t] -> t
        _   -> error $ constrName dc ++ " has no field " ++ n

data RepackCmd
    = Skip Int
    | Repack Int RepackerField Name
    deriving (Show)

fuseSkips :: [RepackCmd] -> [RepackCmd]
fuseSkips (Skip a : Skip b : rcs) = fuseSkips $ Skip (a + b) : rcs
fuseSkips (r      : rcs)          = r : fuseSkips rcs
fuseSkips []                      = []

mkRepackCmds :: DataConstructor -> [(RepackerField, Name)] -> [RepackCmd]
mkRepackCmds dc repacks = fuseSkips $ map mkRepackCmd $ constrFields dc
  where
    mkRepackCmd :: DataField -> RepackCmd
    mkRepackCmd df = fromMaybe (Skip $ getFieldWidth df) $ listToMaybe
        [ Repack (getFieldWidth df) rf n
        | (rf, n) <- repacks
        , fieldName df == Just (repackerFieldName rf)
        ]

executeRepackCmd :: Exp -> RepackCmd -> Q Exp
executeRepackCmd exp (Skip n) =
    [| let (s, ps)      = $(return exp)
           (this, next) = B.splitAt n s
       in (next, ps ++ [this]) |]
executeRepackCmd exp (Repack n (RepackerField _ f) name) =
    [| let (s, ps)      = $(return exp)
           (this, next) = B.splitAt n s
       in (next, ps ++ $(return f) $(return $ VarE name)) |]
