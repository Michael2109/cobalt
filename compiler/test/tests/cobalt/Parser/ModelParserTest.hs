module Parser.ModelParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testModelParser :: Test
testModelParser = do
    let codeModel = "class Test"
    let testModel = testParseSuccess codeModel (Model (Name "Test") ClassModel [] [] Nothing [] [] (BlockStmt [])) modelParser

    let codeExtends = "class Test extends Parent"
    let testExtends = testParseSuccess codeExtends (Model {modelName = Name "Test", modelType = ClassModel, modelModifiers = [], modelFields = [], modelParent = Just (TypeRef (RefLocal (Name "Parent"))), modelParentArguments = [], modelInterfaces = [], modelBody = BlockStmt []}) modelParser

    let codeImplements = "class Test implements Interface"
    let testImplements = testParseSuccess codeImplements (Model {modelName = Name "Test", modelType = ClassModel, modelModifiers = [], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [TypeRef (RefLocal (Name "Interface"))], modelBody = BlockStmt []}) modelParser

    let codeExtendsImplements = "class Test extends Parent implements "
    let testExtendsImplements = testParseSuccess codeExtendsImplements (Model {modelName = Name "Test", modelType = ClassModel, modelModifiers = [], modelFields = [], modelParent = Just (TypeRef (RefLocal (Name "Parent"))), modelParentArguments = [], modelInterfaces = [], modelBody = BlockStmt []}) modelParser

    let codeInner = unlines [ "class OuterClass"
                            , "    class InnerClass"
                            ]
    let testInner = testParseSuccess codeInner (Model {modelName = Name "OuterClass", modelType = ClassModel, modelModifiers = [], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [], modelBody = BlockStmt [ModelDef (Model {modelName = Name "InnerClass", modelType = ClassModel,  modelModifiers = [], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [], modelBody = (BlockStmt [])})]}) modelParser

    let codePureModel = "pure class Test"
    let testPureModel = testParseSuccess codePureModel (Model {modelName = Name "Test", modelType = ClassModel, modelModifiers = [Pure], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [], modelBody = BlockStmt []}) modelParser

    let codeMethods = unlines [ "class OuterClass"
                              , "    let x() = 10"
                              , "    let y() = do"
                              , "        let x() = do"
                              , "            if True then"
                              , "                10"
                              , "            else"
                              , "                20"
                              ]
    let testMethods = testParseSuccess codeMethods (Model {modelName = Name "OuterClass", modelType = ClassModel, modelModifiers = [], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [], modelBody = BlockStmt [MethodDef (Method {methodName = Name "x", methodAnns = [], methodFields = [], methodModifiers = [], methodReturnType = Nothing, methodBody = Inline (IntConst 10)}),MethodDef (Method {methodName = Name "y", methodAnns = [], methodFields = [], methodModifiers = [], methodReturnType = Nothing, methodBody = DoBlock (BlockStmt [MethodDef (Method {methodName = Name "x", methodAnns = [], methodFields = [], methodModifiers = [], methodReturnType = Nothing, methodBody = DoBlock (BlockStmt [If (BoolConst True) (BlockStmt [ExprAsStmt (IntConst 10)]) (Just (BlockStmt [ExprAsStmt (IntConst 20)]))])})])})]}) modelParser


    TestList [ testModel
             , testExtends
             , testImplements
             , testExtendsImplements
             , testInner
             , testPureModel
             , testMethods
             ]
