module Parser.Data.ModelTypeParser where

import Text.Megaparsec

import AST.Data.ModelType
import Parser.BaseParser (rword)
import Parser.ParserType

modelTypeParser :: Parser ModelType
modelTypeParser
    =   ClassModel    <$ rword "class"
    <|> ObjectModel   <$ rword "object"
    <|> TraitModel    <$ rword "trait"
