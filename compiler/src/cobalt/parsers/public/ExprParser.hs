{-|
Module      : ExprParser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser and ABExprParser to generate the AST.
-}
module ExprParser (Parser,
  expr, objectParser, classParser, traitParser, parser) where

import ExprParserPrivate
import ABExprParser
import Block

