module Parser.ArrayOpParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.Parser

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testArrayOpParser :: Test
testArrayOpParser = do
    let code = "x ++ y"
    let test = testParseSuccess code (Array ArrayAppend (Identifier (Name "x")) (Identifier (Name "y"))) expressionParser'

    let codeStringLiteral = "x ++ \"String Literal\""
    let testStringLiteral = testParseSuccess codeStringLiteral (Array ArrayAppend (Identifier (Name "x")) (StringLiteral "String Literal")) expressionParser'

    let codeMultipleStringLiteral = "\"String Literal 1\" ++ \"String Literal 2\""
    let testMultipleStringLiteral = testParseSuccess codeMultipleStringLiteral (Array ArrayAppend (StringLiteral "String Literal 1") (StringLiteral "String Literal 2")) expressionParser'

    TestList [ test
             , testStringLiteral
             , testMultipleStringLiteral
             ]
