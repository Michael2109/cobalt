module Parser.Data.ModifierParser where

import Text.Megaparsec

import AST.Data.Modifier
import Parser.BaseParser (rword)
import Parser.ParserType

accessModifierParser :: Parser Modifier
accessModifierParser
    =   Public    <$ rword "public"
    <|> Protected <$ rword "protected"
    <|> Private   <$ rword "private"

abstractModifierParser :: Parser Modifier
abstractModifierParser = Abstract <$ rword "abstract"

finalModifierParser :: Parser Modifier
finalModifierParser = Final <$ rword "final"