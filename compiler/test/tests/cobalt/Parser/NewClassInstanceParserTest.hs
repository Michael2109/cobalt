module Parser.NewClassInstanceParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testNewClassInstanceParser :: Test
testNewClassInstanceParser = do
    let codeNoArguments = "new ClassName()"
    let testNoArguments = testParseSuccess codeNoArguments (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [])) newClassInstanceParser
    let testNoArgumentsExpr = testParseSuccess codeNoArguments (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [])) expressionParser'

    let codeSingleArgument = "new ClassName(a)"
    let testSingleArgument = testParseSuccess codeSingleArgument (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a")])) newClassInstanceParser
    let testSingleArgumentExpr = testParseSuccess codeSingleArgument (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a")])) expressionParser'

    let codeMultipleArguments = "new ClassName(a, b, c)"
    let testMultipleArguments = testParseSuccess codeMultipleArguments (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])) newClassInstanceParser
    let testMultipleArgumentsExpr = testParseSuccess codeMultipleArguments (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")])) expressionParser'

    TestList [ testNoArguments
             , testNoArgumentsExpr
             , testSingleArgument
             , testSingleArgumentExpr
             , testMultipleArguments
             , testMultipleArgumentsExpr
             ]
