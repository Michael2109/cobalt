module Parser.NumberParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testNumberParser = do

    let codeFloat = "100.50988678f"
    let testFloat = testParseSuccess codeFloat (FloatConst 100.50988678) expressionParser'

    let codeDouble = "100.50988678"
    let testDouble = testParseSuccess codeDouble (DoubleConst 100.50988678) expressionParser'

    let codeInt = "100"
    let testInt = testParseSuccess codeInt (IntConst 100) expressionParser'

    let codeLong = "100l"
    let testLong = testParseSuccess codeLong (LongConst 100) expressionParser'

    TestList [ testFloat
             , testDouble
             , testInt
             , testLong
             ]
