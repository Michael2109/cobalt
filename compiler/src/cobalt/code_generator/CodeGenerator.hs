{-|
Module      : CodeGenerator
Description : Functions for generating code.
-}
module CodeGenerator where

import Block
import IRNode
import SymbolTable

data GeneratedCode = GeneratedCode
  { location :: FilePath -- Where the code will be written to
  , code :: String        -- The generated code
  } deriving (Show)

compileAST :: Expr -> SymbolTable -> String
compileAST ast symbolTable = show $  pretty (genIR ast symbolTable (CurrentState "" ""))  symbolTable (CurrentState "" "")