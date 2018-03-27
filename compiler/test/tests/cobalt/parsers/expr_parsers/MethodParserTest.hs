module MethodParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser

testMethodParser :: Test
testMethodParser = do
  let code = unlines ["exampleMethod (a: Int, b: Int): Int",
                      "  println(\"Hello world\")"
                     ]
  TestCase $ assertEqual code
    (Function (Identifier "exampleMethod") Nothing [ClassParam (Identifier "Int") (Identifier "a"),ClassParam (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
    (case (parse (methodParser "ModuleName" False) "" code) of
      Left  e -> Error
      Right x -> x)