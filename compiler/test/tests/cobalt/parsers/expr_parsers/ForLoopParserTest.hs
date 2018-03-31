module ForLoopParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testForLoopParser :: Test
testForLoopParser = do
  let code = unlines [
        "for(i <- 0 to 10)",
        "  println(i)"
        ]
  TestCase $ assertEqual code
    (For "i" (ArithExpr (IntConst 0)) (ArithExpr (IntConst 10)) [Print (Argument (BooleanExpr (Identifier "i")))])
    (case (parse (forLoopParser) "" code) of
      Left  e -> Error
      Right x -> x)