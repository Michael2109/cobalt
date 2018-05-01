module Parser.ExpressionParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testExpressionParserNested :: Test
testExpressionParserNested = do
    let codeMethodCalls = "methodCall1().methodCall2()"
    let testMethodCalls = TestCase $ assertEqual codeMethodCalls
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr []),MethodCall (Name "methodCall2") (BlockExpr [])])
                           (case (parse expressionParser' "" codeMethodCalls) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMethodCallsSingleArgument = "methodCall1(a).methodCall2(a)"
    let testMethodCallsSingleArgument = TestCase $ assertEqual codeMethodCallsSingleArgument
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a")]),MethodCall (Name "methodCall2") (BlockExpr [Identifier (Name "a")])])
                           (case (parse expressionParser' "" codeMethodCallsSingleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMethodCallsMultipleArguments = "methodCall1(a, b, c).methodCall2(a, b, c)"
    let testMethodCallsMultipleArguments = TestCase $ assertEqual codeMethodCallsMultipleArguments
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]),MethodCall (Name "methodCall2") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])])
                           (case (parse expressionParser' "" codeMethodCallsMultipleArguments) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeIdentifiers = "varName1.varName2"
    let testIdentifiers = TestCase $ assertEqual codeIdentifiers
                           (BlockExpr [Identifier (Name "varName1"),Identifier (Name "varName2")])
                           (case (parse expressionParser' "" codeIdentifiers) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMethodCallsIdentifiers = "methodCall1().varName1"
    let testMethodCallsIdentifiers = TestCase $ assertEqual codeMethodCallsIdentifiers
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr []),Identifier (Name "varName1")])
                           (case (parse expressionParser' "" codeMethodCallsIdentifiers) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMethodCallsIdentifiersSingleArgument = "methodCall1(a).varName1"
    let testMethodCallsIdentifiersSingleArgument = TestCase $ assertEqual codeMethodCallsIdentifiersSingleArgument
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a")]),Identifier (Name "varName1")])
                           (case (parse expressionParser' "" codeMethodCallsIdentifiersSingleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMethodCallsIdentifiersMultipleArguments = "methodCall1(a, b, c).varName1"
    let testMethodCallsIdentifiersMultipleArguments = TestCase $ assertEqual codeMethodCallsIdentifiersMultipleArguments
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]),Identifier (Name "varName1")])
                           (case (parse expressionParser' "" codeMethodCallsIdentifiersMultipleArguments) of
                               Left  e -> error $ show e
                               Right x -> x)

    TestList [ testMethodCalls
             , testMethodCallsSingleArgument
             , testMethodCallsMultipleArguments
             , testIdentifiers
             , testMethodCallsIdentifiers
             , testMethodCallsIdentifiersSingleArgument
             , testMethodCallsIdentifiersMultipleArguments
             ]
