module Parser.MethodParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testMethodParser :: Test
testMethodParser = do
    let codeEmptyParams = "let exampleMethod (): Int = _"
    let testEmptyParams = testParseSuccess codeEmptyParams (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeMultipleParams = "let exampleMethod (a: Int, b: Int): Int = _"
    let testMultipleParams = testParseSuccess codeMultipleParams (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeDoBlock = unlines [ "let outerMethod (): Int = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (MethodDef (Method {methodName = Name "outerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = DoBlock (BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])})) statementParser

    let codeNestedMethod = unlines [ "let outerMethod (): Int = do"
                                   , "    let innerMethod (): Int = do"
                                   , "        i"
                                   , "    j"
                                   ]
    let testNestedMethod = testParseSuccess codeNestedMethod (MethodDef (Method {methodName = Name "outerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = DoBlock $ BlockStmt [MethodDef (Method {methodName = Name "innerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = DoBlock $ BlockStmt [ExprAsStmt (Identifier (Name "i"))]}),ExprAsStmt (Identifier (Name "j"))]})) statementParser

    let codeModifierFinal = "final let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierFinal = testParseSuccess codeModifierFinal (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Final], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeModifierAbstract = "abstract let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierAbstract = testParseSuccess codeModifierAbstract (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Abstract], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeModifierPublic = "public let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierPublic = testParseSuccess codeModifierPublic (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Public], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeModifierProtected = "protected let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierProtected = testParseSuccess codeModifierProtected (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Protected], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeModifierLocal = "local let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierLocal = testParseSuccess codeModifierLocal (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [PackageLocal], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    let codeModifierMultiple = "final abstract public protected local let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierMultiple = testParseSuccess codeModifierMultiple (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Final, Abstract, Public, Protected, PackageLocal], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = Inline (Identifier (Name "_"))})) statementParser

    TestList [ testEmptyParams
             , testMultipleParams
             , testDoBlock
             , testNestedMethod
             , testModifierFinal
             , testModifierAbstract
             , testModifierPublic
             , testModifierProtected
             , testModifierLocal
             , testModifierMultiple
             , testConstructorParser
             ]

testConstructorParser :: Test
testConstructorParser = do
    let codeEmptyParams = "let this () = _"
    let testEmptyParams = testParseSuccess codeEmptyParams (MethodDef (Method {methodName = Name "this", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just Init, methodBody = Inline (Identifier (Name "_"))})) methodDefParser

    let codeMultipleParams = "let this (a: Int, b: Int) = _"
    let testMultipleParams = testParseSuccess codeMultipleParams (MethodDef (Method {methodName = Name "this", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [], methodReturnType = Just Init, methodBody = Inline (Identifier (Name "_"))})) methodDefParser

    let codeDoBlock = unlines [ "let this () = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (MethodDef (Method {methodName = Name "this", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just Init, methodBody = DoBlock (BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])})) methodDefParser

    let codeNestedMethod = unlines [ "let this () = do"
                                   , "    let innerMethod (): Int = do"
                                   , "        i"
                                   , "    j"
                                   ]
    let testNestedMethod = testParseSuccess codeNestedMethod (MethodDef (Method {methodName = Name "this", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just Init, methodBody = DoBlock (BlockStmt [MethodDef (Method {methodName = Name "innerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = Just $ TypeRef (RefLocal (Name "Int")), methodBody = DoBlock (BlockStmt [ExprAsStmt (Identifier (Name "i"))])}),ExprAsStmt (Identifier (Name "j"))])})) methodDefParser

    let codeModifierPublic = "public let this (a: Int, b: Int) = _"
    let testModifierPublic = testParseSuccess codeModifierPublic (MethodDef (Method {methodName = Name "this", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Public], methodReturnType = Just Init, methodBody = Inline (Identifier (Name "_"))})) methodDefParser

    TestList [testEmptyParams, testMultipleParams, testDoBlock, testNestedMethod, testModifierPublic]
