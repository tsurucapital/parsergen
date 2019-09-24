-- | Utilities for automatically deriving parsers and unparsers for certain
-- types
{-# LANGUAGE TemplateHaskell, BangPatterns, TupleSections #-}
module ParserGen.Auto
    ( getFieldParserUnparser
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (liftM2, mplus, replicateM)
import Data.Char (ord)
import Language.Haskell.TH
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)

import ParserGen.Common
import ParserGen.Types
import qualified ParserGen.Parser as P

getFieldParserUnparser :: Int -> DataField -> Int -> Maybe Exp -> Q (Exp, Maybe Exp)
getFieldParserUnparser spaceBefore df spaceAfter mCustomUnparser = do
    (parser, unparser) <- mkFieldParser spaceBefore spaceAfter (fieldParser df)
        (getTypeName $ fieldType df) (fieldWidth df) (getFieldResultIsIgnored df)

    parser'   <- repeatParser df parser
    unparser' <- maybe (return Nothing) (fmap Just . repeatUnparser df) $
        mplus mCustomUnparser unparser
    return (parser', unparser')

mkFieldParser :: Int -> Int -> ParserType -> Name -> Int -> Bool -> Q (Exp, Maybe Exp)
mkFieldParser spaceBefore spaceAfter pty ftyname fwidth fignored = case pty of
        CustomParser p    -> return (p, Nothing)

        UnsignedParser    -> case nameBase ftyname of
            _ | fignored -> wn [|P.unsafeSkip fwidth|]
            "AlphaNum"   -> wj [|unsafeAlphaNum fwidth|] [|putAlphaNum|]
            "ByteString" -> wj [|P.unsafeTake  fwidth|]  [|id|]
            "Char" | fwidth == 1 -> wj [|P.anyChar |]  [|id|]
            "Int"        -> wj (unsafeDecimalXTH fwidth) [|putDecimalX fwidth|]
            x            -> recurse x

        SignedParser      -> case nameBase ftyname of
            _ | fignored -> wn [|P.unsafeSkip fwidth|]
            "Int" -> wj (unsafeDecimalXSTH fwidth) [|putDecimalXS fwidth|]
            x     -> recurse x

        HardcodedString s -> case nameBase ftyname of
            _ | length s /= fwidth -> fail $
                    "Width of " ++ show s ++ " is not " ++ show fwidth ++ "!"

            "StringPattern" -> (,Nothing) <$> stringPatternTH spaceBefore spaceAfter s

            "ByteString"
                -- single byte case, we can match those as Word8 (more efficient) but return as ByteString
                | length s == 1 -> let !w = fromIntegral (ord . head $ s) :: Word8
                                    in wn $ if fignored
                                                then [| P.word8 w |]
                                                else [| P.word8 w >> return (BC.pack s) |]
                -- if string value is ignored - no need to return it
                | fignored  -> wn [| P.string (BC.pack s) |]
                | otherwise -> wn [| P.string (BC.pack s) >> return (BC.pack s) |]

            "Word8"
                -- single byte case
                | length s == 1 -> let !w = fromIntegral (ord . head $ s) :: Word8
                                    in wn $ if fignored
                                                then [| P.word8 w |]
                                                else [| P.word8 w >> return w |]
                -- if string value is ignored - no need to return it
                | fignored  -> wn [| P.string (BC.pack s) |]
                | otherwise -> error "Can't return several values as Word8 - change to ByteString or ignore results"


            e -> error $ "Only ByteString and StringPattern types are supported for hardcoded strings " ++ show e ++ " given"
  where
    recurse ty = do
        (ftyname', cons, uncons) <- getTypeConsUncons ty
        (fparser, funparser)     <- mkFieldParser spaceBefore spaceAfter pty ftyname' fwidth fignored

        let cons' = if cons == VarE 'id
                            then return fparser
                            else [| $(return cons) `fmap` $(return fparser)|]

        liftM2 (,) cons' $
            case funparser of
                Nothing -> return Nothing
                Just f  -> fmap Just [|\x -> $(return f) ($(return uncons) x)|]

    -- | Utility
    wj x y = (,) <$> x <*> fmap Just y
    wn x   = (,) <$> x <*> pure Nothing

-- | The following function takes a type name and generates a proper name,
-- a constructor and an unconstror for it.
--
-- Example: given the type
--
-- > data Wrap = Wrap Int
--
-- We will generate a constructor expression which is equivalent to
--
-- > Wrap :: Int -> Wrap
--
-- and an unconstructor expression equivalent to
--
-- > \w -> let Wrap uw = w in uw :: Wrap -> Int
--
getTypeConsUncons :: String -> Q (Name, Exp, Exp)
getTypeConsUncons name = do
    TyConI info <- recover (fail unknownType) (reify (mkName name))
    id'         <- [|id|]
    case info of
        TySynD _ _ (ConT synTo) ->
            return (synTo, id', id')

        NewtypeD _ _ _ _ (RecC constr [(unconstr, _, ConT typeFor)]) _ ->
            return (typeFor, ConE constr, VarE unconstr)

        NewtypeD _ _ _ _ (NormalC constr [(_, ConT typeFor)]) _ -> do

            -- I don't think there's a simpler way?
            w  <- newName "w"
            uw <- newName "uw"
            let unconstr = LamE [VarP w] (LetE
                    [ValD (ConP constr [VarP uw]) (NormalB (VarE w)) []]
                    (VarE uw))

            return (typeFor, ConE constr, unconstr)

        _ -> fail $
            "Can't deal with " ++ name ++ ", must be a type synonym or newtype"
  where
    unknownType = "Type `" ++ name ++ "' is undefined."

repeatParser :: DataField -> Exp -> Q Exp
repeatParser df p = case fieldRepeat df of
    Nothing -> return p
    Just q  -> [|replicateM q $(return p)|]

repeatUnparser :: DataField -> Exp -> Q Exp
repeatUnparser df up
    | getFieldHasRepeat df = [|map $(return up)|]
    | otherwise            = [|return . $(return up)|]

getTypeName :: Type -> Name
getTypeName (ConT n) = n
getTypeName t        = error $ "Invalid type in size based parser: " ++ show t
