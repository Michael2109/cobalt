module SymbolTable where

data ClassSymbolTable
    = ClassSymbolTable
      {
          className       :: String -- class name
        , publicVariables :: [(String, String)] -- (variable name, variable type name) list of variable
        , methods         :: [(String, MethodSymbolTable)] -- (method name, method symbol) list of methods
      }
      deriving (Eq)
 
instance Show ClassSymbolTable where
    show (ClassSymbolTable cName vars methods) = show cName

data MethodSymbolTable = MethodSymbolTable {
      returnType :: String
    , name       :: String
    , args       :: [(String, String)] -- list of arguments
    }
    deriving (Show, Eq)