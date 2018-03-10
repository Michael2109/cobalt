module IfElseStatementParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

testIfStmtParserBooleanTrue :: Test
testIfStmtParserBooleanTrue = do
  let code = unlines [
        "if(True)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument $ BooleanExpr (BoolConst True)) [Print (Argument $ StringLiteral "Test")])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

testIfStmtParserBooleanFalse :: Test
testIfStmtParserBooleanFalse = do
  let code = unlines [
        "if(True)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument $ BooleanExpr (BoolConst True)) [Print (Argument $ StringLiteral "Test")])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)