module ForLoopParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

testForLoopParser :: Test
testForLoopParser = do
  let code = unlines [
        "for(i <- 0 to 10)",
        "  println(i)"
        ]
  TestCase $ assertEqual code
    (For "i" (ArithExpr (IntConst 0)) (ArithExpr (IntConst 10)) [Print $ Identifier "i"])
    (case (parse (forLoopParser) "" code) of
      Left  e -> Error
      Right x -> x)