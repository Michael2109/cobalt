module Parser.NestedCallParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testNestedCallParser :: Test
testNestedCallParser = do
    let codeNestedMethodCalls = "methodCall1().methodCall2()"
    let testNestedMethodCalls = TestCase $ assertEqual codeNestedMethodCalls
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr []),MethodCall (Name "methodCall2") (BlockExpr [])])
                           (case (parse (nestedCallParser) "" codeNestedMethodCalls) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeNestedIdentifiers = "varName1.varName2"
    let testNestedIdentifiers = TestCase $ assertEqual codeNestedIdentifiers
                           (BlockExpr [Identifier (Name "varName1"),Identifier (Name "varName2")])
                           (case (parse (nestedCallParser) "" codeNestedIdentifiers) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeNestedMethodCallsIdentifiers = "methodCall1().varName1"
    let testNestedMethodCallsIdentifiers = TestCase $ assertEqual codeNestedMethodCallsIdentifiers
                           (BlockExpr [MethodCall (Name "methodCall1") (BlockExpr []),Identifier (Name "varName1")])
                           (case (parse (nestedCallParser) "" codeNestedMethodCallsIdentifiers) of
                               Left  e -> error $ show e
                               Right x -> x)

    TestList [testNestedMethodCalls, testNestedIdentifiers, testNestedMethodCallsIdentifiers]
