module SymbolTableTests where

import Test.HUnit

import SymbolTableTest

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
