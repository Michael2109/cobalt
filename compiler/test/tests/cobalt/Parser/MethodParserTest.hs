module Parser.MethodParserTest where

import Test.HUnit
import Text.Megaparsec
import Text.Megaparsec.Pos
import Data.List.NonEmpty

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testMethodParser :: Test
testMethodParser = do
    let codeEmptyParams = "let exampleMethod (): Int = _"
    let testEmptyParams = testParseSuccess codeEmptyParams (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = ExprAssignment (Identifier (Name "_"))})) methodDefParser

    let codeMultipleParams = "let exampleMethod (a: Int, b: Int): Int = _"
    let testMultipleParams = testParseSuccess codeMultipleParams (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = ExprAssignment (Identifier (Name "_"))})) methodDefParser

    let codeDoBlock = unlines [ "let outerMethod (): Int = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (MethodDef (Method {methodName = Name "outerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = StmtAssignment (BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])})) methodDefParser

    let codeNestedMethod = unlines [ "let outerMethod (): Int = do"
                       , "    let innerMethod (): Int = do"
                       , "        i"
                       , "    j"
                       ]
    let testNestedMethod = testParseSuccess codeNestedMethod (MethodDef (Method {methodName = Name "outerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = StmtAssignment $ BlockStmt [MethodDef (Method {methodName = Name "innerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i"))]}),ExprAsStmt (Identifier (Name "j"))]})) methodDefParser

    let codeModifierPublic = "public let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierPublic = testParseSuccess codeModifierPublic (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Public], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = ExprAssignment (Identifier (Name "_"))})) methodDefParser

    TestList [testEmptyParams, testMultipleParams, testDoBlock, testNestedMethod, testModifierPublic]
