module SymbolTableTest where

import Test.HUnit

import SymbolTable

generateClassSymbolTable1 = ClassSymbolTable "ClassName"
  [("x", "int"),
   ("y", "int"),
   ("z", "double"),
   ("obj", "Object")]
   [("method1", MethodSymbolTable "int" [("i", "int"), ("j", "int"), ("obj", "Object")])]

generateClassSymbolTable2 = ClassSymbolTable "ClassName"
  [("a", "int"),
   ("b", "int"),
   ("c", "double"),
   ("d", "Object")]
   [("method2", MethodSymbolTable "String" [("e", "int"), ("f", "int"), ("g", "Object")]),
    ("method1", MethodSymbolTable "int" [("r", "int"), ("p", "int"), ("u", "Object")])]



generateSymbolTable1 = SymbolTable [generateClassSymbolTable1]

generateSymbolTable2 = SymbolTable [generateClassSymbolTable2]

testSymbolTableExtractReturnType1 :: Test
testSymbolTableExtractReturnType1 = do
  TestCase $ assertEqual "Extract return type"
    "int"
    $ extractReturnType generateSymbolTable1 "ClassName" "method1"

testSymbolTableExtractReturnType2 :: Test
testSymbolTableExtractReturnType2 = do
  TestCase $ assertEqual "Extract return type"
    "String"
    $ extractReturnType generateSymbolTable2 "ClassName" "method2"

testSymbolTableExtractMethodArgs :: Test
testSymbolTableExtractMethodArgs = do
  TestCase $ assertEqual "Extract method args"
    3 $
    length (extractMethodArgs generateSymbolTable2 "ClassName" "method2")

testClassSymbolTableCombine :: Test
testClassSymbolTableCombine = do
  TestCase $ assertEqual "Combine symbol tables"
    ""
    ""

testClassSymbolTableCombineList :: Test
testClassSymbolTableCombineList = do
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
    methodExists generateSymbolTable1 "ClassName" "method1"

testSymbolTableVariableExists:: Test
testSymbolTableVariableExists = do
  TestCase $ assertEqual "Variable exists"
    True $
    instanceVariableExists generateSymbolTable1 "ClassName" "x"

testSymbolTableVariableExistsFail:: Test
testSymbolTableVariableExistsFail = do
  TestCase $ assertEqual "Variable doesn't exists"
    False $
    instanceVariableExists generateSymbolTable1 "ClassName" "unknownVarName"

testSymbolTableGetClassSymbolTable :: Test
testSymbolTableGetClassSymbolTable = do
  TestCase $ assertEqual "Get class symbol table from symbol table"
    "ClassName" $
    className (getClassSymbolTable generateSymbolTable1 "ClassName")