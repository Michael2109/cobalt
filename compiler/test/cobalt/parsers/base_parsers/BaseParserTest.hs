module BaseParserTest where

import Test.HUnit

import Text.Megaparsec

import BaseParser

-- Symbols

testSymbolSingle :: Test
testSymbolSingle = do
  let code = "%"
  TestCase $ assertEqual code
    "%"
    (case (parse (symbol "%") "" code) of
      Left  e -> "Error"
      Right x -> x)

testSymbolMultiple :: Test
testSymbolMultiple = do
  let code = "#@$"
  TestCase $ assertEqual code
    "#@$"
    (case (parse (symbol "#@$") "" code) of
      Left  e -> "Error"
      Right x -> x)

testSymbolFail :: Test
testSymbolFail = do
  let code = "^&*"
  TestCase $ assertEqual code
    "Error"
    (case (parse (symbol "#@") "" code) of
      Left  e -> "Error"
      Right x -> x)

-- Reserved word

testReservedWord :: Test
testReservedWord = do
  let code = "module"
  TestCase $ assertEqual code
    "module"
    (case (parse (rword "module") "" code) of
      Left  e -> "Error"
      Right x -> x)

-- Identifier

testIdentifier :: Test
testIdentifier = do
  let code = "identifier"
  TestCase $ assertEqual code
    "identifier"
    (case (parse identifier "" code) of
      Left  e -> "Error"
      Right x -> x)

testIdentifierFail :: Test
testIdentifierFail = do
  let code = "123identifier"
  TestCase $ assertEqual code
    "Error"
    (case (parse identifier "" code) of
      Left  e -> "Error"
      Right x -> x)
