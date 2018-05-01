module Parser.MethodParserTest where

import Test.HUnit
import Text.Megaparsec
import Text.Megaparsec.Pos
import Data.List.NonEmpty


import AST.AST
import Parser.ExprParser

testMethodParser :: Test
testMethodParser = do
    let codeEmptyParams = "let exampleMethod (): Int = _"
    let testEmptyParams = TestCase $ assertEqual codeEmptyParams
                          (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = ExprAssignment (Identifier (Name "_"))}))
                          (case (parse methodDefParser "" codeEmptyParams) of
                              Left  e -> error $ show e
                              Right x -> x)

    let codeMultipleParams = "let exampleMethod (a: Int, b: Int): Int = _"
    let testMultipleParams = TestCase $ assertEqual codeMultipleParams
                                (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = ExprAssignment (Identifier (Name "_"))}))
                                (case (parse methodDefParser "" codeMultipleParams) of
                                     Left  e -> error $ show e
                                     Right x -> x)

    let codeDoBlock = unlines [ "let outerMethod (): Int = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = TestCase $ assertEqual codeDoBlock
                          (MethodDef (Method {methodName = Name "outerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = StmtAssignment (BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])}))
                          (case (parse methodDefParser "" codeDoBlock) of
                              Left  e -> error $ show e
                              Right x -> x)

    let codeNestedMethod = unlines [ "let outerMethod (): Int = do"
                       , "    let innerMethod (): Int = do"
                       , "        i"
                       , "    j"
                       ]
    let testNestedMethod = TestCase $ assertEqual codeNestedMethod
                               (MethodDef (Method {methodName = Name "outerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = StmtAssignment $ BlockStmt [MethodDef (Method {methodName = Name "innerMethod", methodAnns = [], methodParams = [], methodModifiers = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i"))]}),ExprAsStmt (Identifier (Name "j"))]}))
                               (case (parse methodDefParser "" codeNestedMethod) of
                                   Left  e -> error $ show e
                                   Right x -> x)

    -- Modifiers
    let codeModifierPublic = "member let exampleMethod (a: Int, b: Int): Int = _"
    let testModifierPublic = TestCase $ assertEqual codeModifierPublic
                                (MethodDef (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = Just (TypeRef (RefLocal (Name "Int"))), fieldInit = Nothing}], methodModifiers = [Public], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = ExprAssignment (Identifier (Name "_"))}))
                                (case (parse (methodDefParser) "" codeModifierPublic) of
                                     Left  e -> error (show e)
                                     Right x -> x)

    TestList [testEmptyParams, testMultipleParams, testDoBlock, testNestedMethod, testModifierPublic]
