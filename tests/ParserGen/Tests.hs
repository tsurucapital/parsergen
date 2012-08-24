{-# LANGUAGE OverloadedStrings #-}
module ParserGen.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import ParserGen.Parser
import ParserGen.Tests.Packet

tests :: Test
tests = testGroup "ParserGen.Tests"
    [ testCase "parse sampleWarning"    testParseSampleWarning
    , testCase "parse sampleLotteryWin" testParseSampleLotteryWin
    , testCase "parse sampleMessage"    testParseSampleMessage
    , testCase "width sampleMessage"    testWidthForMessage
    , testCase "repackerForWarning"     testRepackerForWarning
    , testCase "repackerForLotteryWin"  testRepackerForLotteryWin
    , testCase "testRepackerForMessage" testRepackerForMessage
    ]

testParseSampleWarning :: Assertion
testParseSampleWarning =
    -- A 'RobotUprising' with only 2 percent of survival?! At least it's not an
    -- 'AngryGirlfriend'...
    Right (Warning RobotUprising 2) @=?
    parse parserForPacket sampleWarning

testParseSampleLotteryWin :: Assertion
testParseSampleLotteryWin =
    Right (LotteryWin (Money 9999999999) [4, 8, 15, 16, 23, 42]) @=?
    parse parserForPacket sampleLotteryWin

testParseSampleMessage :: Assertion
testParseSampleMessage =
    Right (Message "CATS    " (-20) "ALL YOUR BASE ARE BELONG TO US  ") @=?
    parse parserForMessage sampleMessage

testWidthForMessage :: Assertion
testWidthForMessage = 62 @=? widthForMessage

testRepackerForWarning :: Assertion
testRepackerForWarning =
    "WARNZA002" @=? repackerForWarning ZombieApocalypse sampleWarning

testRepackerForLotteryWin :: Assertion
testRepackerForLotteryWin = case parse parserForPacket packet' of
    Left err -> fail err
    Right x  -> [1 .. 6] @=? lwWinningEntry x
  where
    packet' = repackerForLotteryWin [1 .. 6] sampleLotteryWin

testRepackerForMessage :: Assertion
testRepackerForMessage = 
    "MESSGLADOS  IMPORTANT 0945-020PLEASE CONTINUE TESTING         " @=?
    repackerForMessage "GLADOS  " "PLEASE CONTINUE TESTING         "
        sampleMessage
