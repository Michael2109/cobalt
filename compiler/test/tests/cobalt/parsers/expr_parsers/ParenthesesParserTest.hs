module ParenthesesParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser


testParenthesesVar :: Test
testParenthesesVar = do
  let code = "(x)"
  TestCase $ assertEqual code
    (Identifier "x")
    (case (parse (parens aExpr) "" code) of
      Left e -> Error
      Right x -> x)

testParenthesesNested :: Test
testParenthesesNested = do
  let code = "(((((x)))))"
  TestCase $ assertEqual code
    (Identifier "x")
    (case (parse (parens aExpr) "" code) of
      Left e -> Error
      Right x -> x)

testParenthesesNoOpenFail :: Test
testParenthesesNoOpenFail = do
  let code = "x)"
  TestCase $ assertEqual code
    Error
    (case (parse (parens aExpr) "" code) of
      Left e -> Error
      Right x -> x)

testParenthesesNoCloseFail :: Test
testParenthesesNoCloseFail = do
  let code = "(x"
  TestCase $ assertEqual code
    Error
    (case (parse (parens aExpr) "" code) of
      Left e -> Error
      Right x -> x)

-- TODO more complex nesting patterns