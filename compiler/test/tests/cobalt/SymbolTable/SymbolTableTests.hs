module SymbolTable.SymbolTableTests where

import Test.HUnit

import SymbolTable.SymbolTableTest

symbolTableTestList :: Test
symbolTableTestList = TestList
    [ testSymbolTableExtractReturnType1
    , testSymbolTableExtractReturnType2
    , testSymbolTableExtractMethodArgs
    , testSymbolTableCombine
    , testSymbolTableCombineList
    , testClassSymbolTableCombine
    , testClassSymbolTableCombineList
    , testSymbolTableMethodExists
    , testSymbolTableVariableExists
    , testSymbolTableVariableType
    , testSymbolTableVariableExistsFail
    , testSymbolTableGetClassSymbolTable
    ]
