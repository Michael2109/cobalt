module SymbolTable where

import Data.List

data SymbolTable
  = SymbolTable
    {
       classSymbolTables :: [ClassSymbolTable] -- All class symbol tables for all classes to be compiled
    }
    deriving (Eq)

data ClassSymbolTable
    = ClassSymbolTable
      {
          className       :: String -- class name
        , publicVariables :: [(String, String)] -- (variable name, variable type name) list of variable
        , methods         :: [(String, MethodSymbolTable)] -- (method name, method symbol) list of methods
      }
      deriving (Eq)


instance Show SymbolTable where
    show (SymbolTable classSymbolTables) = intercalate "" (map show classSymbolTables)

instance Show ClassSymbolTable where
    show (ClassSymbolTable cName vars methods) = show cName ++ intercalate " " (map show vars) ++ intercalate " " (map show methods)

data MethodSymbolTable = MethodSymbolTable {
      returnType :: String
    , methodArgs       :: [(String, String)] -- list of arguments
    }
    deriving (Show, Eq)

data CurrentState
  = CurrentState {
    currentClassName :: String,
    currentMethodName :: String
  }
  deriving (Eq)