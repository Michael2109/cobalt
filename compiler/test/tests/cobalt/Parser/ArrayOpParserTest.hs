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

    let codeMultipleStringLiteral = "\"String Literal 1\" ++ x"
    let testMultipleStringLiteral = testParseSuccess codeMultipleStringLiteral (Array ArrayAppend (StringLiteral "String Literal 1")  (Identifier (Name "x"))) expressionParser'

    let codeMultipleMixed = "\"String Literal 1\" ++ ClassName.varName ++ methodCall() ++ varName"
    let testMultipleMixed = testParseSuccess codeMultipleMixed (Array ArrayAppend (Array ArrayAppend (Array ArrayAppend (StringLiteral "String Literal 1") (BlockExpr [Identifier (Name "ClassName"),Identifier (Name "varName")])) (MethodCall (Name "methodCall") (BlockExpr []))) (Identifier (Name "varName"))) expressionParser'

    let codeParenthesis = "(\"String Literal 1\" ++ ClassName.varName) ++ methodCall() ++ varName"
    let testParenthesis = testParseSuccess codeParenthesis (Array ArrayAppend (Array ArrayAppend (Array ArrayAppend (StringLiteral "String Literal 1") (BlockExpr [Identifier (Name "ClassName"),Identifier (Name "varName")])) (MethodCall (Name "methodCall") (BlockExpr []))) (Identifier (Name "varName"))) expressionParser'

    TestList [ testArrayOp
             , testStringLiteral
             , testMultipleStringLiteral
             , testMultipleMixed
             , testParenthesis
             ]
