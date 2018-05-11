module Parser.ArrayOpParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testArrayOpParser :: Test
testArrayOpParser = do
    let codeArrayOp = "x ++ y"
    let testArrayOp = testParseSuccess codeArrayOp (Array ArrayAppend (Identifier (Name "x")) (Identifier (Name "y"))) expressionParser'

    let codeStringLiteral = "x ++ \"String Literal\""
    let testStringLiteral = testParseSuccess codeStringLiteral (Array ArrayAppend (Identifier (Name "x")) (StringLiteral "String Literal")) expressionParser'

    let codeMultipleStringLiteral = "\"String Literal 1\"++ x"
    let testMultipleStringLiteral = testParseSuccess codeMultipleStringLiteral (Array ArrayAppend (StringLiteral "String Literal 1")  (Identifier (Name "x"))) expressionParser'

    TestList [ testArrayOp
             , testStringLiteral
             , testMultipleStringLiteral
             ]
