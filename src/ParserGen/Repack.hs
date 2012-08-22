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
import Data.Maybe (fromMaybe, listToMaybe)
import Language.Haskell.TH

import ParserGen.ParseQuote
import ParserGen.Types

genRepackFromFile :: FilePath -> Q [Dec]
genRepackFromFile templateName = do
    (dts, rs) <- unzipDecls <$> getDecls templateName
    fmap concat $ mapM (mkRepacker dts) rs

mkRepacker :: [Datatype] -> Repacker -> Q [Dec]
mkRepacker dts (Repacker rname cname cfields) = do
    withNames <- mapM (\cf -> (,) cf <$> newName "p") cfields
    let repackCmds = mkRepackCmds dc withNames

    bsVar     <- newName "bs"
    undef     <- [|undefined|]
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

mkRepackCmds :: DataConstructor -> [(RepackerField, Name)] -> [RepackCmd]
mkRepackCmds dc repacks = fuseSkips $ map mkRepackCmd $ constrFields dc
  where
    mkRepackCmd :: DataField -> RepackCmd
    mkRepackCmd df = fromMaybe (Skip $ getFieldWidth df) $ listToMaybe
        [ Repack df (getUnparser rf) n
        | (rf, n) <- repacks
        , fieldName df == Just (repackerFieldName rf)
        ]

    getUnparser (RepackerField _ (Just up)) = up
    getUnparser _                           = error "No unparser found"

executeRepackCmd :: Exp -> RepackCmd -> Q Exp
executeRepackCmd e (Skip n) =
    [| let (s, ps)      = $(return e)
           (this, next) = B.splitAt n s
       in (next, ps ++ [this]) |]
executeRepackCmd e (Repack df f name) = do
    repeatedF <- if r then [|map $(return f)|] else [|return . $(return f)|]
    [| let (s, ps)         = $(return e)
           (this, next)    = B.splitAt n s
       in (next, ps ++ $(return repeatedF) $(return $ VarE name)) |]
  where
    n = getFieldWidth df
    r = getFieldHasRepeat df
