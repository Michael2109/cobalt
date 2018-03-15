module CodeGenerator where

import ABBlock
import Block
import SymbolTable

data GeneratedCode = GeneratedCode {
    location :: [String], -- Where the code will be written to
    code :: String        -- The generated code
    }

compileAST ast symbolTable =
  case ast of
    Left  e -> show e
    Right x -> genCode x symbolTable (CurrentState "" "")