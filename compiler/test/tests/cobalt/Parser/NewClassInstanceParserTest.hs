module Parser.NewClassInstanceParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testNewClassInstanceParser :: Test
testNewClassInstanceParser = do
    let codeNoArguments = "new ClassName()"
    let testNoArguments = TestCase $ assertEqual codeNoArguments
                           (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr []))
                           (case (parse newClassInstanceParser "" codeNoArguments) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeSingleArgument = "new ClassName(a)"
    let testSingleArgument = TestCase $ assertEqual codeSingleArgument
                           (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a")]))
                           (case (parse newClassInstanceParser "" codeSingleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMultipleArgument = "new ClassName(a, b, c)"
    let testMultipleArgument = TestCase $ assertEqual codeMultipleArgument
                           (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]))
                           (case (parse newClassInstanceParser "" codeMultipleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    TestList [testNoArguments, testSingleArgument, testMultipleArgument]

testNewClassInstanceParserExpr :: Test
testNewClassInstanceParserExpr = do
    let codeNoArguments = "new ClassName()"
    let testNoArguments = TestCase $ assertEqual codeNoArguments
                           (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr []))
                           (case (parse expressionParser' "" codeNoArguments) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeSingleArgument = "new ClassName(a)"
    let testSingleArgument = TestCase $ assertEqual codeSingleArgument
                           (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a")]))
                           (case (parse expressionParser' "" codeSingleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    let codeMultipleArgument = "new ClassName(a, b, c)"
    let testMultipleArgument = TestCase $ assertEqual codeMultipleArgument
                           (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]))
                           (case (parse expressionParser' "" codeMultipleArgument) of
                               Left  e -> error $ show e
                               Right x -> x)

    TestList [testNoArguments, testSingleArgument, testMultipleArgument]

