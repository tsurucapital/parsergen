{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module ParserGen.Tests.Packet
    ( DangerType (..)
    , Money (..)
    , LotteryEntry
    , MessageBody (..)

    , Packet (..)
    , parserForWarning
    , parserForLotteryWin
    , parserForMessage
    , parserForPacket

    , widthForWarning
    , widthForLotteryWin
    , widthForMessage

    , repackerForWarning
    , repackerForLotteryWin
    , repackerForMessage

    , sampleWarning
    , sampleLotteryWin
    , sampleMessage
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import GHC.Exts (IsString)
import ParserGen.Gen
import ParserGen.Repack
import qualified ParserGen.Parser as P

data DangerType
    = Earthquake
    | ZombieApocalypse
    | RobotUprising
    | AngryGirlfriend
    deriving (Eq, Show)

dangerType :: P.Parser DangerType
dangerType = do
    bs <- P.take 2
    case bs of
        "EQ" -> return Earthquake
        "ZA" -> return ZombieApocalypse
        "RI" -> return RobotUprising
        "AG" -> return AngryGirlfriend
        _    -> fail $ "Unknown danger type: " ++ show bs

unDangerType :: DangerType -> ByteString
unDangerType Earthquake       = "EQ"
unDangerType ZombieApocalypse = "ZA"
unDangerType RobotUprising    = "RI"
unDangerType AngryGirlfriend  = "AG"

newtype Money = Money Int
    deriving (Eq, Show)

type LotteryEntry = Int

newtype MessageBody = MessageBody ByteString
    deriving (Eq, IsString, Show)

$(genDataTypeFromFile "Packet.ths")
$(genParserFromFile   "Packet.ths")
$(genWidthFromFile    "Packet.ths")
$(genRepackFromFile   "Packet.ths")

parserForPacket :: P.Parser Packet
parserForPacket = parserForWarning <|> parserForLotteryWin <|> parserForMessage

sampleWarning :: ByteString
sampleWarning = "WARNRI002"

sampleLotteryWin :: ByteString
sampleLotteryWin = "LOTT9999999999040815162342"

sampleMessage :: ByteString
sampleMessage = "MESSCATS    IMPORTANT 0945-020ALL YOUR BASE ARE BELONG TO US  "
