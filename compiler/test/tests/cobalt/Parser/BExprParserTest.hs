module Parser.BExprParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testBParser :: Test
testBParser = do
    let codeTrue = "True"
    let testTrue = testParseSuccess codeTrue (BoolConst True) expressionParser'

    let codeFalse = "False"
    let testFalse = testParseSuccess codeFalse (BoolConst False) expressionParser'

    let codeParens = "(True)"
    let testParens = testParseSuccess codeParens (BoolConst True) expressionParser'

    let codeR = "x < 10"
    let testR = testParseSuccess codeR (RBinary Less (Identifier (Name "x")) (IntConst 10)) expressionParser'

    TestList [ testTrue
             , testFalse
             , testParens
             , testR
             ]

testRParser :: Test
testRParser = do
    let codeGreaterThan = "x > 10"
    let testGreaterThan = testParseSuccess codeGreaterThan (RBinary Greater (Identifier (Name "x")) (IntConst 10)) expressionParser'

    let codeLessThan = "x < 10"
    let testLessThan = testParseSuccess codeLessThan (RBinary Less (Identifier (Name "x")) (IntConst 10)) expressionParser'

    let codeGreaterThanEqual = "x >= 10"
    let testGreaterThanEqual = testParseSuccess codeGreaterThanEqual (RBinary GreaterEqual (Identifier (Name "x")) (IntConst 10)) expressionParser'

    let codeLessThanEqual = "x <= 10"
    let testLessThanEqual = testParseSuccess codeLessThanEqual (RBinary LessEqual (Identifier (Name "x")) (IntConst 10)) expressionParser'

    TestList [ testGreaterThan
             , testLessThan
             , testGreaterThanEqual
             , testLessThanEqual
             ]
