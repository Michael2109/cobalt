module SymbolTableTests where

import Test.HUnit

import SymbolTable

import SymbolTableTest

symbolTableTestList = TestList [
    testSymbolTableExtractReturnType1,
    testSymbolTableExtractReturnType2,
    testSymbolTableExtractMethodArgs,
    testSymbolTableCombine,
    testSymbolTableCombineList,
    testSymbolTableVariableExist,
    testSymbolTableGetClassSymbolTable
  ]


