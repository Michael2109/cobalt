module Parser.StringLiteralParserTest where

import Test.HUnit
import Text.Megaparsec


import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testStringLiteralParser :: Test
testStringLiteralParser = do

    let codeStringLiteralSimple = "\"foo\""
    let testStringLiteralSimple = testParseSuccess codeStringLiteralSimple (StringLiteral "foo") stringLiteralParser

    let codeStringLiteralSimpleWhitespace = "\"foo   bar\tbaz\""
    let testStringLiteralSimpleWhitespace = testParseSuccess codeStringLiteralSimpleWhitespace (StringLiteral "foo   bar\tbaz") stringLiteralParser

    let codeStringLiteralEscapeTab = "\"\\tfoo\""
    let testStringLiteralEscapeTab = testParseSuccess  codeStringLiteralEscapeTab (StringLiteral "\tfoo") stringLiteralParser

    let codeStringLiteralEmpty = "\"\""
    let testStringLiteralEmpty = testParseSuccess codeStringLiteralEmpty (StringLiteral "") stringLiteralParser

    let codeStringLiteralNewLine = "\"foo\\n\""
    let testStringLiteralNewLine = testParseSuccess codeStringLiteralNewLine (StringLiteral "foo\n") stringLiteralParser

    let codeStringLiteralMultipleNewLine = "\"foo\\nbar\\nbaz\""
    let testStringLiteralMultipleNewLine = testParseSuccess codeStringLiteralMultipleNewLine (StringLiteral "foo\nbar\nbaz") stringLiteralParser

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
    let codeStringLiteralUnescapedSingleQuote = "\"foo\'\""
    let testStringLiteralUnescapedSingleQuote = testParseSuccess codeStringLiteralUnescapedSingleQuote (StringLiteral "foo\'") stringLiteralParser

    let codeStringLiteralEscapedSingleQuote  = "\"foo\\\'\""
    let testStringLiteralEscapedSingleQuote = testParseSuccess codeStringLiteralEscapedSingleQuote (StringLiteral "foo\'") stringLiteralParser

    let codeStringLiteralEscapedDoubleQuote = "\"foo\\\"ending\""
    let testStringLiteralEscapedDoubleQuote = testParseSuccess codeStringLiteralEscapedDoubleQuote (StringLiteral "foo\"ending") stringLiteralParser

    let codeStringLiteralDoubleQuoteMultiple = "\"test:\\\"string inside string\\\" ending\""
    let testStringLiteralDoubleQuoteMultiple = testParseSuccess codeStringLiteralDoubleQuoteMultiple (StringLiteral "test:\"string inside string\" ending") stringLiteralParser

{--
testStringLiteralUnfinishedMultilineFail :: Test
testStringLiteralUnfinishedMultilineFail = do
  let code = "\"\'\'\'\nfoo\n"
--}

    let codeStringLiteralUnfinishedFail = "\"ufinishedstring\n"
    let testStringLiteralUnfinishedFail = testParseFailure codeStringLiteralUnfinishedFail stringLiteralParser

{--
testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail :: Test
testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail = do
    let code =  "\"first line \n \t\t\tsecond line\""
    TestCase $ assertEqual code
        (Error)
        (case (parse (stringLiteral) "" code) of
             Left  _ -> Error
             Right x -> x)
--}

{--
testStringLiteralUnfinishedDoubleLineFail :: Test
testStringLiteralUnfinishedDoubleLineFail = do
    let code = "\"first line \n second line\""
    TestCase $ assertEqual code
        (Error)
        (case (parse (stringLiteral) "" code) of
             Left  _ -> Error
             Right x -> x)
--}

{-- after resolving multiline literal semantics more tests for leading whitespace need to be added --}
    TestList [ testStringLiteralSimple
             , testStringLiteralSimpleWhitespace
             , testStringLiteralEscapeTab
             , testStringLiteralEmpty
             , testStringLiteralNewLine
             , testStringLiteralMultipleNewLine
             , testStringLiteralUnescapedSingleQuote
             , testStringLiteralEscapedSingleQuote
             , testStringLiteralEscapedDoubleQuote
             , testStringLiteralDoubleQuoteMultiple
             , testStringLiteralUnfinishedFail
             ]
