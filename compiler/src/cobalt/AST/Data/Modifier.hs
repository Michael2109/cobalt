module AST.Data.Modifier where

data Modifier
    = Public
    | Protected
    | Private
    | Abstract
    | Final
        deriving (Eq, Show)