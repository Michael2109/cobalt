module Parser.ExpressionParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testExpressionParserNested :: Test
testExpressionParserNested = do
    let codeMethodCalls = "methodCall1().methodCall2()"
    let testMethodCalls = testParseSuccess codeMethodCalls (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr []),MethodCall (Name "methodCall2") (BlockExpr [])]) expressionParser'

    let codeMethodCallsSingleArgument = "methodCall1(a).methodCall2(a)"
    let testMethodCallsSingleArgument = testParseSuccess codeMethodCallsSingleArgument (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a")]),MethodCall (Name "methodCall2") (BlockExpr [Identifier (Name "a")])]) expressionParser'

    let codeMethodCallsMultipleArguments = "methodCall1(a, b, c).methodCall2(a, b, c)"
    let testMethodCallsMultipleArguments = testParseSuccess codeMethodCallsMultipleArguments (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]),MethodCall (Name "methodCall2") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])]) expressionParser'

    let codeIdentifiers = "varName1.varName2"
    let testIdentifiers = testParseSuccess codeIdentifiers (BlockExpr [Identifier (Name "varName1"),Identifier (Name "varName2")]) expressionParser'

    let codeMethodCallsIdentifiers = "methodCall1().varName1"
    let testMethodCallsIdentifiers = testParseSuccess codeMethodCallsIdentifiers (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr []),Identifier (Name "varName1")]) expressionParser'

    let codeMethodCallsIdentifiersSingleArgument = "methodCall1(a).varName1"
    let testMethodCallsIdentifiersSingleArgument = testParseSuccess codeMethodCallsIdentifiersSingleArgument (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a")]),Identifier (Name "varName1")]) expressionParser'

    let codeMethodCallsIdentifiersMultipleArguments = "methodCall1(a, b, c).varName1"
    let testMethodCallsIdentifiersMultipleArguments = testParseSuccess codeMethodCallsIdentifiersMultipleArguments (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]),Identifier (Name "varName1")]) expressionParser'

    TestList [ testMethodCalls
             , testMethodCallsSingleArgument
             , testMethodCallsMultipleArguments
             , testIdentifiers
             , testMethodCallsIdentifiers
             , testMethodCallsIdentifiersSingleArgument
             , testMethodCallsIdentifiersMultipleArguments
             ]
