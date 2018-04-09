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
    , testModelSymbolTableCombine
    , testModelSymbolTableCombineList
    , testSymbolTableMethodExists
    , testSymbolTableVariableExists
    , testSymbolTableVariableType
    , testSymbolTableVariableExistsFail
    , testSymbolTableGetModelSymbolTable
    ]
