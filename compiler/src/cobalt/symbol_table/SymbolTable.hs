module SymbolTable where

data ClassSymbol
    = ClassSymbol
      {
          className       :: String -- class name
        , publicVariables :: [(String, String)] -- (variable name, variable type name) list of variable
        , methods         :: [(String, MethodSymbol)] -- (method name, method symbol) list of methods
      }
      deriving (Eq)
 
instance Show ClassSymbol where
    show (ClassSymbol cName vars methods) = show cName

data MethodSymbol = MethodSymbol {
      returnType :: String
    , name       :: String
    , args       :: [(String, String)] -- list of arguments
    }
    deriving (Show, Eq)