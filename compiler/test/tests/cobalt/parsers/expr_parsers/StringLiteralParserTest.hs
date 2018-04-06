module StringLiteralParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testStringLiteralSimple :: Test
testStringLiteralSimple = do
    let code = "\"foo\""
    TestCase $ assertEqual code
        (StringLiteral "foo")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralSimpleWhitespace :: Test
testStringLiteralSimpleWhitespace = do
    let code = "\"foo   bar\tbaz\""
    TestCase $ assertEqual code
        (StringLiteral "foo   bar\tbaz")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralEscapeTab :: Test
testStringLiteralEscapeTab = do
    let code = "\"\\tfoo\""
    TestCase $ assertEqual code
        (StringLiteral "\tfoo")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralEmpty :: Test
testStringLiteralEmpty = do
    let code = "\"\""
    TestCase $ assertEqual code
        (StringLiteral "")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralNewLine :: Test
testStringLiteralNewLine = do
    let code = "\"foo\\n\""
    TestCase $ assertEqual code
        (StringLiteral "foo\n")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralMultipleNewLine :: Test
testStringLiteralMultipleNewLine = do
    let code = "\"foo\\nbar\\nbaz\""
    TestCase $ assertEqual code
        (StringLiteral "foo\nbar\nbaz")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

{--
testStringLiteralMultiLineSimple :: Test
testStringLiteralMultiLineSimple  = do
  let code = "\'\'\'\nfoo\n\'\'\'"

testStringLiteralMultiLineEmptyLineMixed :: Test
testStringLiteralMultiLineEmptyLineMixed  = do
  let code = "\'\'\'\nfoo\n\nbar\n\'\'\'"

testStringLiteralMultiLineEmptyString :: Test
testStringLiteralMultiLineEmptyString  = do
  let code = "\'\'\'\n\'\'\'"

testStringLiteralMultiLineEmptyLine :: Test
testStringLiteralMultiLineEmptyLine  = do
  let code = "\'\'\'\n\n\'\'\'"

testStringLiteralMultiLineMultiple :: Test
testStringLiteralMultiLineMultiple  = do
  let code = "\'\'\'\nfoo\nb a r\nbaz\'\'\'"
--}
testStringLiteralUnescapedSingleQuote :: Test
testStringLiteralUnescapedSingleQuote = do
    let code = "\"foo\'\""
    TestCase $ assertEqual code
        (StringLiteral "foo\'")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralEscapedSingleQuote :: Test
testStringLiteralEscapedSingleQuote = do
    let code = "\"foo\\\'\""
    TestCase $ assertEqual code
        (StringLiteral "foo\'")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

testStringLiteralEscapedDoubleQuote :: Test
testStringLiteralEscapedDoubleQuote = do
    let code = "\"foo\\\"ending\""
    TestCase $ assertEqual code
        (StringLiteral "foo\"ending")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)


testStringLiteralDoubleQuoteMultiple :: Test
testStringLiteralDoubleQuoteMultiple = do
    let code = "\"test:\\\"string inside string\\\" ending\""
    TestCase $ assertEqual code
        (StringLiteral "test:\"string inside string\" ending")
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

{--
testStringLiteralUnfinishedMultilineFail :: Test
testStringLiteralUnfinishedMultilineFail = do
  let code = "\"\'\'\'\nfoo\n"
--}

testStringLiteralUnfinishedFail :: Test
testStringLiteralUnfinishedFail = do
    let code = "\"ufinishedstring\n"
    TestCase $ assertEqual code
        (Error)
        (case (parse (stringLiteralParser) "" code) of
             Left  e -> Error
             Right x -> x)

{--
testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail :: Test
testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail = do
    let code =  "\"first line \n \t\t\tsecond line\""
    TestCase $ assertEqual code
        (Error)
        (case (parse (stringLiteral) "" code) of
             Left  e -> Error
             Right x -> x)
--}

{--
testStringLiteralUnfinishedDoubleLineFail :: Test
testStringLiteralUnfinishedDoubleLineFail = do
    let code = "\"first line \n second line\""
    TestCase $ assertEqual code
        (Error)
        (case (parse (stringLiteral) "" code) of
             Left  e -> Error
             Right x -> x)
--}

{-- after resolving multiline literal semantics more tests for leading whitespace need to be added --}
