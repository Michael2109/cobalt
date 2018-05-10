module Parser.BaseParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import Parser.BaseParser

testSymbol :: Test
testSymbol = do
    let codeSingle = "%"
    let testSingle = testParseSuccess codeSingle "%" (symbol "%")

    let codeMultiple = "#@$"
    let testMultiple = testParseSuccess codeMultiple "#@$" (symbol "#@$")

    TestList [ testSingle
             , testMultiple
             ]

testReservedWord :: Test
testReservedWord = do
    let codeRWord = "module"
    let testRWord  = testParseSuccess codeRWord  "module" (rword "module")

    TestList [testRWord]

testIdentifier :: Test
testIdentifier = do
    let codeIdentifier = "identifier"
    let testIdentifierConst = testParseSuccess codeIdentifier "identifier" identifier
    TestList [testIdentifierConst]

testFloat :: Test
testFloat = do
    let codeFloat = "100.50988678f"
    let testFloatConst = testParseSuccess codeFloat 100.50988678 floatParser
    TestList [testFloatConst]

testDouble :: Test
testDouble = do
    let codeDouble = "100.50988678"
    let testDoubleConst = testParseSuccess codeDouble 100.50988678 doubleParser
    TestList [testDoubleConst]

testInteger :: Test
testInteger = do
    let codeInteger = "100"
    let testIntegerConst = testParseSuccess codeInteger 100 integerParser
    TestList [testIntegerConst]

testLong :: Test
testLong = do
    let codeLong = "100l"
    let testLongConst = testParseSuccess codeLong 100 longParser
    TestList [testLongConst]
