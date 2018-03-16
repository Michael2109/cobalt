module CodeGenerator where

import ABBlock
import Block
import SymbolTable

data GeneratedCode = GeneratedCode {
    location :: FilePath, -- Where the code will be written to
    code :: String        -- The generated code
    }
    deriving (Show)


compileAST ast symbolTable =
  genCode ast symbolTable (CurrentState "" "")