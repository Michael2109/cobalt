module AST.Data.ModelType where

data ModelType
    = ClassModel
    | ObjectModel
    | TraitModel
        deriving (Eq, Show)