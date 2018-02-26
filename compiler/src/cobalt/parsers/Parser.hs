{-|
Module      : Parser
Description : Functions that call the ExprParser functions to generate the AST.
-}
module Parser where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import ExprParser
import SymbolTable

parseTree relativeDir input = parse (objectParser (ClassSymbolTable "ClassName" [] []) relativeDir <|> classParser (ClassSymbolTable "ClassName" [] []) relativeDir) "" input

parseString relativeDir input =
  case parse (objectParser (ClassSymbolTable "ClassName" [] []) relativeDir <|> classParser (ClassSymbolTable "ClassName" [] []) relativeDir) "" input of
    Left  e -> show e
    Right x -> show x

parsePrint :: [String] -> String -> IO()
parsePrint relativeDir input = parseTest' (objectParser (ClassSymbolTable "ClassName" [] []) relativeDir <|> classParser (ClassSymbolTable "ClassName" [] []) relativeDir) input

parseFromFile file = runParser (expr (ClassSymbolTable "ClassName" [] [])) file <$> readFile file