module BaseParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

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

-- Numbers
testFloat :: Test
testFloat = do
  let code = "100.50988678f"
  TestCase $ assertEqual code
    (100.50988678)
    (case (parse doubleParser "" code) of
      Left  e -> -1
      Right x -> x)

testDouble :: Test
testDouble = do
  let code = "100.50988678"
  TestCase $ assertEqual code
    (100.50988678)
    (case (parse doubleParser "" code) of
      Left  e -> -1
      Right x -> x)

testInteger :: Test
testInteger = do
  let code = "100"
  TestCase $ assertEqual code
    (100)
    (case (parse integerParser "" code) of
      Left  e -> -1
      Right x -> x)

testLong :: Test
testLong = do
  let code = "100l"
  TestCase $ assertEqual code
    (100)
    (case (parse integerParser "" code) of
      Left  e -> -1
      Right x -> x)