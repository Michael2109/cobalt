module SymbolTable.SymbolTableTest where

import Test.HUnit

import AST.AST
import SymbolTable.SymbolTable

generateModelSymbolTable1 :: ModelSymbolTable
generateModelSymbolTable1 = ModelSymbolTable "ModelName" ClassModel []
    [("x", "int")
    , ("y", "int")
    , ("z", "double")
    , ("obj", "Object")]
    [("method1", MethodSymbolTable "int" [("i", "int"), ("j", "int"), ("obj", "Object")])]

generateModelSymbolTable2 :: ModelSymbolTable
generateModelSymbolTable2 = ModelSymbolTable "ModelName" ClassModel []
    [("a", "int")
    , ("b", "int")
    , ("c", "double")
    , ("d", "Object")]
    [("method2", MethodSymbolTable "String" [("e", "int"), ("f", "int"), ("g", "Object")])
    , ("method1", MethodSymbolTable "int" [("r", "int"), ("p", "int"), ("u", "Object")])]

generateSymbolTable1 :: SymbolTable
generateSymbolTable1 = SymbolTable [generateModelSymbolTable1]

generateSymbolTable2 :: SymbolTable
generateSymbolTable2 = SymbolTable [generateModelSymbolTable2]

testSymbolTableExtractReturnType1 :: Test
testSymbolTableExtractReturnType1 = do
    TestCase $ assertEqual "Extract return type"
        "int"
        $ extractReturnType generateSymbolTable1 "ModelName" "method1"

testSymbolTableExtractReturnType2 :: Test
testSymbolTableExtractReturnType2 = do
    TestCase $ assertEqual "Extract return type"
        "String"
        $ extractReturnType generateSymbolTable2 "ModelName" "method2"

testSymbolTableExtractMethodArgs :: Test
testSymbolTableExtractMethodArgs = do
    TestCase $ assertEqual "Extract method args"
        3 $
        length (extractMethodArgs generateSymbolTable2 "ModelName" "method2")

testModelSymbolTableCombine :: Test
testModelSymbolTableCombine = do
    TestCase $ assertEqual "Combine symbol tables"
        ""
        ""

testModelSymbolTableCombineList :: Test
testModelSymbolTableCombineList = do
    TestCase $ assertEqual "Combine list of symbol tables"
        ""
        ""

testSymbolTableCombine :: Test
testSymbolTableCombine = do
    TestCase $ assertEqual "Combine symbol tables"
        ""
        ""

testSymbolTableCombineList :: Test
testSymbolTableCombineList = do
    TestCase $ assertEqual "Combine list of symbol tables"
        ""
        ""

testSymbolTableMethodExists :: Test
testSymbolTableMethodExists = do
    TestCase $ assertEqual "Method exists"
        True $
        methodExists generateSymbolTable1 "ModelName" "method1"

testSymbolTableVariableExists :: Test
testSymbolTableVariableExists = do
    TestCase $ assertEqual "Variable exists"
        True $
        instanceVariableExists generateSymbolTable1 "ModelName" "x"

testSymbolTableVariableType :: Test
testSymbolTableVariableType = do
    TestCase $ assertEqual "Variable type"
        "double" $
        case (instanceVariableType generateSymbolTable2 "ModelName" "c") of
            Just varType -> varType
            Nothing -> "Error"


testSymbolTableVariableExistsFail:: Test
testSymbolTableVariableExistsFail = do
    TestCase $ assertEqual "Variable doesn't exist"
        False $
        instanceVariableExists generateSymbolTable1 "ModelName" "unknownVarName"

testSymbolTableGetModelSymbolTable :: Test
testSymbolTableGetModelSymbolTable = do
    TestCase $ assertEqual "Get model symbol table from symbol table"
        "ModelName" $
        case getModelSymbolTable generateSymbolTable1 "ModelName" of
            Just a -> modelName (a)
            Nothing -> ""
