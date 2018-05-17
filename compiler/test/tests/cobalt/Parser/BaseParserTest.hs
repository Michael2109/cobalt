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
    let code1 = "module"
    let test1 = testParseSuccess code1 "module" (rword "module")

    TestList [test1]

testIdentifier :: Test
testIdentifier = do
    let code1 = "identifier"
    let test1 = testParseSuccess code1 "identifier" identifier
    TestList [test1]

testFloat :: Test
testFloat = do
    let code1 = "100.50988678f"
    let test1 = testParseSuccess code1 100.50988678 floatParser
    TestList [test1]

testDouble :: Test
testDouble = do
    let code1 = "100.50988678"
    let test1 = testParseSuccess code1 100.50988678 doubleParser
    TestList [test1]

testInteger :: Test
testInteger = do
    let code1 = "100"
    let test1 = testParseSuccess code1 100 integerParser
    TestList [test1]

testLong :: Test
testLong = do
    let code1 = "100l"
    let test1 = testParseSuccess code1 100 longParser
    TestList [test1]
