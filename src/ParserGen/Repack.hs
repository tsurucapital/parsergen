{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Repack
    ( genRepackFromFile
    ) where

import Control.Applicative
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (find)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH

import ParserGen.Auto
import ParserGen.ParseQuote
import ParserGen.Types

genRepackFromFile :: FilePath -> Q [Dec]
genRepackFromFile templateName = do
    (dts, rs) <- unzipDecls <$> getDecls templateName
    fmap concat $ mapM (mkRepacker dts) rs

mkRepacker :: [Datatype] -> Repacker -> Q [Dec]
mkRepacker dts (Repacker rname cname cfields) = do
    withNames  <- mapM (\cf -> (,) cf <$> newName "p") cfields
    repackCmds <- mkRepackCmds dc withNames

    bsVar     <- newName "bs"
    btbT      <- [t|ByteString -> ByteString|]
    foldInit  <- [|($(varE bsVar) , [])|]
    fold      <- foldM executeRepackCmd foldInit repackCmds
    result    <- [|B.concat $ snd $(return fold)|]
    resultVar <- newName "result"

    validLen   <- [|B.length $(varE resultVar) == $(litE $ integerL len)|]
    otherwise' <- [|otherwise|]

    return
        [ SigD repackerName (foldr mkType btbT cfields)
        , FunD repackerName
            [ Clause
                (map (VarP . snd) withNames ++ [VarP bsVar])
                (GuardedB
                    -- Return original BS if length test fails. Note that we
                    -- only check the total length, where we actually also could
                    -- check the length of each field...
                    [ (NormalG validLen,   VarE resultVar)
                    , (NormalG otherwise', VarE bsVar)
                    ])
                [ValD (VarP resultVar) (NormalB result) []]
            ]
        ]
  where
    repackerName = mkName rname
    len          = fromIntegral $ getConstructorWidth dc

    dc = case [c | dt <- dts, c <- typeConstrs dt, constrName c == cname] of
        [x] -> x
        _   -> error $ "No genparser for constructor " ++ cname

    mkType (RepackerField name _) t =
        AppT (AppT ArrowT (getFieldType name dc)) t

getFieldType :: String -> DataConstructor -> Type
getFieldType n dc =
    case [getFieldRepeatType f | f <- constrFields dc, fieldName f == Just n] of
        [t] -> t
        _   -> error $ constrName dc ++ " has no field " ++ n

data RepackCmd
    = Skip Int
    | Repack DataField Exp Name
    deriving (Show)

fuseSkips :: [RepackCmd] -> [RepackCmd]
fuseSkips (Skip a : Skip b : rcs) = fuseSkips $ Skip (a + b) : rcs
fuseSkips (r      : rcs)          = r : fuseSkips rcs
fuseSkips []                      = []

mkRepackCmds :: DataConstructor -> [(RepackerField, Name)] -> Q [RepackCmd]
mkRepackCmds dc repacks = fmap fuseSkips $ mapM mkRepackCmd $ constrFields dc
  where
    mkRepackCmd :: DataField -> Q RepackCmd
    mkRepackCmd df@(DataField {..}) =
        case find ((== fieldName) . Just . repackerFieldName . fst) repacks of
            Nothing      -> return $ Skip $ getFieldWidth df
            Just (rf, n) -> do
                -- Try to automatically derive an unparser with the optionally
                -- custom-specified one
                (_, unparser) <- getFieldParserUnparser df
                    (repackerFieldUnparser rf)

                -- Compose the two
                let unparser' = fromMaybe
                        (error $ "No unparser found for " ++ show fieldName)
                        unparser

                return $ Repack df unparser' n

executeRepackCmd :: Exp -> RepackCmd -> Q Exp
executeRepackCmd e (Skip n) =
    [| let (s, ps)      = $(return e)
           (this, next) = B.splitAt n s
       in (next, ps ++ [this]) |]
executeRepackCmd e (Repack df f name) = do
    [| let (s, ps)   = $(return e)
           (_, next) = B.splitAt n s
       in (next, ps ++ $(return f) $(return $ VarE name)) |]
  where
    n = getFieldWidth df
