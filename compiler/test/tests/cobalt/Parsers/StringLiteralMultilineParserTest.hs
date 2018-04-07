module Parsers.StringLiteralMultilineParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

{--
testStringLiteralMultilineSimple :: Test
testStringLiteralMultilineSimple = do
  let code = "```foo```"
  TestCase $ assertEqual code
    (StringLiteral "foo")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineSimpleWhitespace :: Test
testStringLiteralMultilineSimpleWhitespace = do
  let code = "```foo   bar\tbaz```"
  TestCase $ assertEqual code
    (StringLiteral "foo   bar\tbaz")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineEscapeTab :: Test
testStringLiteralMultilineEscapeTab = do
  let code = "```\\tfoo```"
  TestCase $ assertEqual code
    (StringLiteral "\tfoo")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineEmpty :: Test
testStringLiteralMultilineEmpty = do
  let code = "```````"
  TestCase $ assertEqual code
    (StringLiteral "")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineNewLine :: Test
testStringLiteralMultilineNewLine = do
  let code = "```foo\\n```"
  TestCase $ assertEqual code
    (StringLiteral "foo\n")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineMultipleNewLine :: Test
testStringLiteralMultilineMultipleNewLine = do
  let code = "```foo\\nbar\\nbaz```"
  TestCase $ assertEqual code
    (StringLiteral "foo\nbar\nbaz")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineUnescapedSingleQuote :: Test
testStringLiteralMultilineUnescapedSingleQuote = do
  let code = "```foo\'```"
  TestCase $ assertEqual code
    (StringLiteral "foo\'")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineEscapedSingleQuote :: Test
testStringLiteralMultilineEscapedSingleQuote = do
  let code = "```foo\\\'```"
  TestCase $ assertEqual code
    (StringLiteral "foo\'")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineEscapedDoubleQuote :: Test
testStringLiteralMultilineEscapedDoubleQuote = do
  let code = "```foo\\\"ending```"
  TestCase $ assertEqual code
    (StringLiteral "foo\"ending")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineDoubleQuoteMultiple :: Test
testStringLiteralMultilineDoubleQuoteMultiple = do
  let code = "```test:\\\"string inside string\\\" ending```"
  TestCase $ assertEqual code
    (StringLiteral "test:\"string inside string\" ending")
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineUnfinishedFail :: Test
testStringLiteralMultilineUnfinishedFail = do
  let code = "```ufinishedstring\n"
  TestCase $ assertEqual code
    (Error)
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

testStringLiteralMultilineExcludingLeft :: Test
testStringLiteralMultilineExcludingLeft = do
  let code = "   ```\\n   This is   \\na multiline   \\nstring   ```"
  TestCase $ assertEqual code
    (StringLiteral $ unlines ["This is",
                              "a multiline",
                              "string"
                              ])
    (case (parse (stringLiteralMultilineParser) "" code) of
      Left  _ -> Error
      Right x -> x)

--}
