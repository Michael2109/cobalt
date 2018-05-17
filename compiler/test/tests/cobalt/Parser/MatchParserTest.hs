module Parser.MethodCallParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testMethodCallParser :: Test
testMethodCallParser = do
    let codeNoArguments = "methodCall()"
    let testNoArguments = testParseSuccess codeNoArguments (MethodCall (Name "methodCall") (BlockExpr [])) expressionParser'

    let codeSingleArgument = "methodCall(a)"
    let testSingleArgument = testParseSuccess codeSingleArgument (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a")])) expressionParser'

    let codeMultipleArgument = "methodCall(a, b, c)"
    let testMultipleArgument = testParseSuccess codeMultipleArgument (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])) expressionParser'

    TestList [ testNoArguments
             , testSingleArgument
             , testMultipleArgument
             ]
