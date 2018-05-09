module Parser.MethodCallParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testMethodCallParser :: Test
testMethodCallParser = do
    let codeNoArguments = "methodCall()"
    let testNoArguments = testParseSuccess codeNoArguments (MethodCall (Name "methodCall") (BlockExpr [])) methodCallParser
    let testNoArgumentsExpr = testParseSuccess codeNoArguments (MethodCall (Name "methodCall") (BlockExpr [])) expressionParser'

    let codeSingleArgument = "methodCall(a)"
    let testSingleArgument = testParseSuccess codeSingleArgument (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a")])) methodCallParser
    let testSingleArgumentExpr = testParseSuccess codeSingleArgument (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a")])) expressionParser'

    let codeMultipleArgument = "methodCall(a, b, c)"
    let testMultipleArgument = testParseSuccess codeMultipleArgument (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])) methodCallParser
    let testMultipleArgumentExpr = testParseSuccess codeMultipleArgument (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])) expressionParser'

    TestList [ testNoArguments
             , testNoArgumentsExpr
             , testSingleArgument
             , testSingleArgumentExpr
             , testMultipleArgument
             , testMultipleArgumentExpr
             ]
