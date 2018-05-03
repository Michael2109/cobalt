module Parser.BaseParserTest where

import Test.HUnit

import Text.Megaparsec

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
    let code = "module"
    let test = testParseSuccess code "module" (rword "module")

    TestList [test]

testIdentifier :: Test
testIdentifier = do
    let code = "identifier"
    let test = testParseSuccess code "identifier" identifier
    TestList [test]

testFloat :: Test
testFloat = do
    let code = "100.50988678f"
    let test = testParseSuccess code 100.50988678 floatParser
    TestList [test]

testDouble :: Test
testDouble = do
    let code = "100.50988678"
    let test = testParseSuccess code 100.50988678 doubleParser
    TestList [test]

testInteger :: Test
testInteger = do
    let code = "100"
    let test = testParseSuccess code 100 integerParser
    TestList [test]

testLong :: Test
testLong = do
    let code = "100l"
    let test = testParseSuccess code 100 longParser
    TestList [test]
