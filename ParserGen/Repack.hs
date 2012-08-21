{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Repack
    ( genRepackFromFile

    , putDecimalX
    , putDecimalXS
    , putTS8
    ) where

import Control.Applicative
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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
        [ Repack df (repackerFieldUnparser rf) n
        | (rf, n) <- repacks
        , fieldName df == Just (repackerFieldName rf)
        ]

executeRepackCmd :: Exp -> RepackCmd -> Q Exp
executeRepackCmd e (Skip n) =
    [| let (s, ps)      = $(return e)
           (this, next) = B.splitAt n s
       in (next, ps ++ [this]) |]
executeRepackCmd e (Repack df f name) = do
    repeatedF <- if r then [|concatMap $(return f)|] else return f
    [| let (s, ps)         = $(return e)
           (this, next)    = B.splitAt n s
       in (next, ps ++ $(return repeatedF) $(return $ VarE name)) |]
  where
    n = getFieldWidth df
    r = getFieldHasRepeat df


putDecimalX :: Int -> Int -> [ByteString]
putDecimalX l i = [BC.pack $ putDecimalX_S l i]

putDecimalXS :: Int ->  Int -> [ByteString]
putDecimalXS l i
    | i >= 0    = [BC.pack $ ' ' : putDecimalX_S l i]
    | otherwise = [BC.pack $ '-' : putDecimalX_S l (negate i)]

putTS8 :: Int -> Int -> Int -> Int -> [ByteString]
putTS8 h m s u = map BC.pack
    [ putDecimalX_S 2 h
    , putDecimalX_S 2 m
    , putDecimalX_S 2 s
    , putDecimalX_S 2 u
    ]

-- helper function
putDecimalX_S :: Int -> Int -> String
putDecimalX_S l i
    | i >= 0    = reverse . take l . reverse $ (replicate l '0' ++ show i)
    | otherwise =
        error "ParserGen.Repack: Can't put negative decimal X: " ++ show i
