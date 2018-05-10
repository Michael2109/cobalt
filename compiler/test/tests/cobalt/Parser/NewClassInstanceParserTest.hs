module Parser.NewClassInstanceParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testNewClassInstanceParser :: Test
testNewClassInstanceParser = do
    let codeNoArguments = "new ClassName()"
    let testNoArguments = testParseSuccess codeNoArguments (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr []) Nothing) expressionParser'

    let codeSingleArgument = "new ClassName(a)"
    let testSingleArgument = testParseSuccess codeSingleArgument (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a")]) Nothing) expressionParser'

    let codeMultipleArguments = "new ClassName(a, b, c)"
    let testMultipleArguments = testParseSuccess codeMultipleArguments (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]) Nothing) expressionParser'

    let codeAnonymousClass = unlines [ "new ClassName()"
                                     , "    let valName = 100"
                                     , "    let x(): Int = 10"
                                     , "    let y(): Int = 20"
                                     ]
    let testAnonymousClass = testParseSuccess codeAnonymousClass (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr []) (Just (BlockStmt [Assign (Name "valName") Nothing True (ExprAssignment (IntConst 100)),MethodDef (Method {methodName = Name "x", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just (TypeRef (RefLocal (Name "Int"))), methodBody = ExprAssignment (IntConst 10)}),MethodDef (Method {methodName = Name "y", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just (TypeRef (RefLocal (Name "Int"))), methodBody = ExprAssignment (IntConst 20)})]))) expressionParser'

    TestList [ testNoArguments
             , testSingleArgument
             , testMultipleArguments
             , testAnonymousClass
             ]
