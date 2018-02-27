module CodeGenerator where

import ABBlock
import Block

compileAST ast symbolTable =
  case ast of
    Left  e -> show e
    Right x -> genCode x symbolTable