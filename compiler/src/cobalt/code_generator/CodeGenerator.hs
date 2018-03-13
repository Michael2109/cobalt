module CodeGenerator where

import ABBlock
import Block
import SymbolTable

compileAST ast symbolTable =
  case ast of
    Left  e -> show e
    Right x -> genCode x symbolTable (CurrentState "" "")