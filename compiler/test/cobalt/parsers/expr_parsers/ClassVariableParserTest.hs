module ClassVariableParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

testClassParser :: Test
testClassParser = do
  let code = unlines [
        "class Test extends Other implements Other"
        ]
  TestCase $ assertEqual code
    --Class [String]String (Maybe [Expr]) (Maybe String) (Maybe String) [Expr] [Expr] [Expr] [Expr]
    --Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray

    (Class [] "Test" (Nothing) (Nothing) Nothing [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)