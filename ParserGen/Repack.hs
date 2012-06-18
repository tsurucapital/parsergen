{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module ParserGen.Repack
    ( genRepackFromFile
    , putDecimalX
    , putDecimalXS
    , putTS8
    ) where

import ParserGen.ParseQuote

import Control.Applicative
import Control.Monad (guard, foldM)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH as TH
import qualified Data.ByteString.Char8 as C8


data ToRepack
    = ToRepack
    { trPacketName :: String -- to generate name repackerForXXXXXXX
    , trPrefix     :: String -- to validate correctness of the packet
    , trPacketSize :: Int    -- to validate correctness of the packet
    , trPieces     :: [PackElement]
    } deriving (Show)

data PackElement
    = PackBS Int -- leave this number of bytes as it is
    | PackBids Int String
    | PackAsks Int String
--    | PackIssue
--    | PackTS
    deriving (Show)



putDecimalX :: Int -> Int -> C8.ByteString
putDecimalX l i = C8.pack $ putDecimalX_S l i

putDecimalXS :: Int ->  Int -> C8.ByteString
putDecimalXS l i | i >= 0 = C8.pack $ ' ' : putDecimalX_S l i
putDecimalXS l i | otherwise = C8.pack $ '-' : putDecimalX_S l (negate i)

putTS8 :: Int -> Int -> Int -> Int -> C8.ByteString
putTS8 h m s u = C8.pack $ concat [ putDecimalX_S 2 h
                                  , putDecimalX_S 2 m
                                  , putDecimalX_S 2 s
                                  , putDecimalX_S 2 u
                                  ]
-- helper function
putDecimalX_S :: Int -> Int -> String
putDecimalX_S l i | i >= 0 = reverse . take l . reverse $ (replicate l '0' ++ show i)
putDecimalX_S _ i = error "ParserGen.Repack: Can't put negative decimal X: " ++ show i



genRepackFromFile :: FilePath -> Q [Dec]
genRepackFromFile templateName = getDatatype templateName >>= mkModifiers


mkModifiers :: Datatype -> Q [Dec]
mkModifiers dt = mapM mkRepacker (getRepack dt) >>= return . concat


mkRepacker :: ToRepack -> Q [Dec]
mkRepacker (ToRepack {..}) = do
        -- validation function - string length and prefix should match
        val <- [| \b -> trPacketSize == C8.length b && (C8.pack trPrefix) `C8.isPrefixOf` b |]

        -- type signature for declared functions
        sig <- [t| C8.ByteString -> [(Int, Int)] -> [(Int, Int)] -> C8.ByteString |]
        foldInit <- [| ( $(return $ VarE pkt) , []) |]

        -- after some magical process we produce the actual bytestring result
        -- C8.concat should be replaced with blazebuilder
        bs <- [| C8.concat $ snd $(foldM processPiece foldInit trPieces) |]

        let modify = (NormalG $ AppE val (VarE pkt), bs)
            keep = (NormalG $ VarE 'otherwise, VarE pkt)

        let fun = [Clause pat (GuardedB [modify, keep]) []]
        return [SigD name sig, FunD name fun]
    where

        processPiece :: Exp -> PackElement -> Q Exp-- {{{
        processPiece e (PackBS l) =
            [| let (s, ps)      = $(return e)
                   (this, next) = C8.splitAt l s
               in (next, ps ++ [this]) |]

        processPiece e (PackBids i f) = do
            [| let (s, ps) = $(return e)
                   next = C8.drop i s
               in (next, ps ++ [ $(return . VarE . mkName $ f) $(return $ VarE bids) ] ) |]

        processPiece e (PackAsks i f) = do
            [| let (s, ps) = $(return e)
                   next = C8.drop i s
               in (next, ps ++ [ $(return . VarE . mkName $ f) $(return $ VarE asks) ] ) |]
        {-
        processPiece e PackTS = do
            [| let (s, ps) = $(return e)
                   next = C8.drop 8 s
               in (next, ps ++ [ $(return $ VarE ts) ]) |]-- }}}

        processPiece e PackIssue = do
            [| let (s, ps) = $(return e)
                   next = C8.drop 12 s
                in (next, ps ++ [ $(return $ VarE issue) ] ) |]
        -}



        asks  = mkName "asks"   :: Name
        bids  = mkName "bids"   :: Name
        pkt   = mkName "pkt"    :: Name
        --issue = mkName "issue"  :: Name
        --ts    = mkName "ts"     :: Name

        pat :: [Pat]
        --pat = [ VarP pkt, VarP issue, VarP asks, VarP bids, VarP ts ]
        pat = [ VarP pkt, VarP asks, VarP bids ]


        name = mkName $ "repackerFor" ++ trPacketName :: Name

        -- 1) validate base packet if it requires any modifications at all
        -- 2) split packet into [ByteString], modifying PackIssue/PackAsks/PackBids/PackTS
        -- 3) join them back with blaze builder


getRepack :: Datatype -> [ToRepack]
getRepack =  mapMaybe maybeRepack . typeConstrs
    where

        maybeRepack :: DataConstructor -> Maybe ToRepack
        maybeRepack dc = do
            let fs = constrFields dc
                trPacketName = constrName dc
                trPacketSize = sum . map getFieldWidth $ fs
            (DataField Nothing Nothing _ _ _ (HardcodedString trPrefix): _) <- Just fs
            trPieces <- fusePackBS <$> mapM toPiece fs
            guard $ and [ any isAsks fs
                        , any isBids fs
                        -- , any isIssueCode fs
                        -- , any isTS fs
                        ]
            return ToRepack {..}

        fusePackBS :: [PackElement] -> [PackElement]
        fusePackBS (PackBS a : PackBS b : ps) = fusePackBS (PackBS (a+b) : ps)
        fusePackBS (p:ps) = p : fusePackBS ps
        fusePackBS [] = []

        toPiece :: DataField -> Maybe PackElement
        toPiece df = case () of
            _ | isAsks df -> PackAsks (getFieldWidth df) <$> (("bs_" ++) <$> getCustomParser df)
              | isBids df -> PackBids (getFieldWidth df) <$> (("bs_" ++) <$> getCustomParser df)
            --  | isIssueCode df -> Just PackIssue
            --  | isTS df        -> Just PackTS
              | otherwise      -> Just $ PackBS (getFieldWidth df)

        getCustomParser :: DataField -> Maybe String
        getCustomParser df = case fieldParser df of
                                CustomParser (VarE n) -> Just $ nameBase n
                                _ -> Nothing

        isAsks :: DataField -> Bool
        isAsks (DataField (Just "BestAsks") (Just 5) _ _ _ (CustomParser _)) = True
        isAsks _ = False

        isBids :: DataField -> Bool
        isBids (DataField (Just "BestBids") (Just 5) _ _ _ (CustomParser _)) = True
        isBids _ = False

        {-
        isTS :: DataField -> Bool
        isTS f = case getCustomParser f of
                    Just "ts8" -> True
                    _ -> False

        isIssueCode :: DataField -> Bool
        isIssueCode (DataField (Just "IssueCode") _ _ _ _ _) = True
        isIssueCode _ = False
        -}
