module Parser.MethodCallParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testMethodCallParser :: Test
testMethodCallParser = do
    let codeNoArguments = "methodCall()"
    let testNoArguments = testParseSuccess codeNoArguments (MethodCall (Name "methodCall") (BlockExpr [])) methodCallParser

    let codeSingleArgument = "methodCall(a)"
    let testSingleArgument = TestCase $ assertEqual codeSingleArgument
                           (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a")]))
                           (case (parse (methodCallParser) "" codeSingleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMultipleArgument = "methodCall(a, b, c)"
    let testMultipleArgument = TestCase $ assertEqual codeMultipleArgument
                           (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]))
                           (case (parse (methodCallParser) "" codeMultipleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    TestList [testNoArguments, testSingleArgument, testMultipleArgument]

testMethodCallParserExpr :: Test
testMethodCallParserExpr = do
    let codeNoArguments = "methodCall()"
    let testNoArguments = TestCase $ assertEqual codeNoArguments
                        (MethodCall (Name "methodCall") (BlockExpr []))
                        (case (parse expressionParser' "" codeNoArguments) of
                             Left  e -> error $ show e
                             Right x -> x)

    let codeSingleArgument = "methodCall(a)"
    let testSingleArgument = TestCase $ assertEqual codeSingleArgument
                           (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a")]))
                           (case (parse expressionParser' "" codeSingleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMultipleArgument = "methodCall(a, b, c)"
    let testMultipleArgument = TestCase $ assertEqual codeMultipleArgument
                           (MethodCall (Name "methodCall") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]))
                           (case (parse expressionParser' "" codeMultipleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    TestList [testNoArguments, testSingleArgument, testMultipleArgument]
