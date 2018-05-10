module Parser.StringLiteralParserTest where

import Test.HUnit


import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testStringLiteralParser :: Test
testStringLiteralParser = do

    let codeStringLiteralSimple = "\"foo\""
    let testStringLiteralSimple = testParseSuccess codeStringLiteralSimple (StringLiteral "foo") stringLiteralParser
    let testStringLiteralSimpleExpr = testParseSuccess codeStringLiteralSimple (StringLiteral "foo") expressionParser

    let codeStringLiteralSimpleWhitespace = "\"foo   bar\tbaz\""
    let testStringLiteralSimpleWhitespace = testParseSuccess codeStringLiteralSimpleWhitespace (StringLiteral "foo   bar\tbaz") stringLiteralParser
    let testStringLiteralSimpleWhitespaceExpr = testParseSuccess codeStringLiteralSimpleWhitespace (StringLiteral "foo   bar\tbaz") expressionParser

    let codeStringLiteralEscapeTab = "\"\\tfoo\""
    let testStringLiteralEscapeTab = testParseSuccess  codeStringLiteralEscapeTab (StringLiteral "\tfoo") stringLiteralParser
    let testStringLiteralEscapeTabExpr = testParseSuccess  codeStringLiteralEscapeTab (StringLiteral "\tfoo") expressionParser

    let codeStringLiteralEmpty = "\"\""
    let testStringLiteralEmpty = testParseSuccess codeStringLiteralEmpty (StringLiteral "") stringLiteralParser
    let testStringLiteralEmptyExpr = testParseSuccess codeStringLiteralEmpty (StringLiteral "") expressionParser

    let codeStringLiteralNewLine = "\"foo\\n\""
    let testStringLiteralNewLine = testParseSuccess codeStringLiteralNewLine (StringLiteral "foo\n") stringLiteralParser
    let testStringLiteralNewLineExpr = testParseSuccess codeStringLiteralNewLine (StringLiteral "foo\n") expressionParser

    let codeStringLiteralMultipleNewLine = "\"foo\\nbar\\nbaz\""
    let testStringLiteralMultipleNewLine = testParseSuccess codeStringLiteralMultipleNewLine (StringLiteral "foo\nbar\nbaz") stringLiteralParser
    let testStringLiteralMultipleNewLineExpr = testParseSuccess codeStringLiteralMultipleNewLine (StringLiteral "foo\nbar\nbaz") expressionParser

    let codeStringLiteralUnescapedSingleQuote = "\"foo\'\""
    let testStringLiteralUnescapedSingleQuote = testParseSuccess codeStringLiteralUnescapedSingleQuote (StringLiteral "foo\'") stringLiteralParser
    let testStringLiteralUnescapedSingleQuoteExpr = testParseSuccess codeStringLiteralUnescapedSingleQuote (StringLiteral "foo\'") expressionParser

    let codeStringLiteralEscapedSingleQuote  = "\"foo\\\'\""
    let testStringLiteralEscapedSingleQuote = testParseSuccess codeStringLiteralEscapedSingleQuote (StringLiteral "foo\'") stringLiteralParser
    let testStringLiteralEscapedSingleQuoteExpr = testParseSuccess codeStringLiteralEscapedSingleQuote (StringLiteral "foo\'") expressionParser

    let codeStringLiteralEscapedDoubleQuote = "\"foo\\\"ending\""
    let testStringLiteralEscapedDoubleQuote = testParseSuccess codeStringLiteralEscapedDoubleQuote (StringLiteral "foo\"ending") stringLiteralParser
    let testStringLiteralEscapedDoubleQuoteExpr = testParseSuccess codeStringLiteralEscapedDoubleQuote (StringLiteral "foo\"ending") expressionParser

    let codeStringLiteralDoubleQuoteMultiple = "\"test:\\\"string inside string\\\" ending\""
    let testStringLiteralDoubleQuoteMultiple = testParseSuccess codeStringLiteralDoubleQuoteMultiple (StringLiteral "test:\"string inside string\" ending") stringLiteralParser
    let testStringLiteralDoubleQuoteMultipleExpr = testParseSuccess codeStringLiteralDoubleQuoteMultiple (StringLiteral "test:\"string inside string\" ending") expressionParser

    let codeStringLiteralUnfinishedMultilineFail = "\"\'\'\'\nfoo\n"
    let testStringLiteralUnfinishedMultilineFail = testParseFailure codeStringLiteralUnfinishedMultilineFail stringLiteralParser
    let testStringLiteralUnfinishedMultilineFailExpr = testParseFailure codeStringLiteralUnfinishedMultilineFail expressionParser

    let codeStringLiteralUnfinishedFail = "\"ufinishedstring\n"
    let testStringLiteralUnfinishedFail = testParseFailure codeStringLiteralUnfinishedFail stringLiteralParser
    let testStringLiteralUnfinishedFailExpr = testParseFailure codeStringLiteralUnfinishedFail expressionParser

    let codeStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail =  "\"first line \n \t\t\tsecond line\""
    let testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail = testParseFailure codeStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail stringLiteralParser
    let testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFailExpr = testParseFailure codeStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail expressionParser

    let codeStringLiteralUnfinishedDoubleLineFail = "\"first line \n second line\""
    let testStringLiteralUnfinishedDoubleLineFail = testParseFailure codeStringLiteralUnfinishedDoubleLineFail stringLiteralParser
    let testStringLiteralUnfinishedDoubleLineFailExpr = testParseFailure codeStringLiteralUnfinishedDoubleLineFail expressionParser

    TestList [ testStringLiteralSimple
             , testStringLiteralSimpleExpr
             , testStringLiteralSimpleWhitespace
             , testStringLiteralSimpleWhitespaceExpr
             , testStringLiteralEscapeTab
             , testStringLiteralEscapeTabExpr
             , testStringLiteralEmpty
             , testStringLiteralEmptyExpr
             , testStringLiteralNewLine
             , testStringLiteralNewLineExpr
             , testStringLiteralMultipleNewLine
             , testStringLiteralMultipleNewLineExpr
             , testStringLiteralUnescapedSingleQuote
             , testStringLiteralUnescapedSingleQuoteExpr
             , testStringLiteralEscapedSingleQuote
             , testStringLiteralEscapedSingleQuoteExpr
             , testStringLiteralEscapedDoubleQuote
             , testStringLiteralEscapedDoubleQuoteExpr
             , testStringLiteralDoubleQuoteMultiple
             , testStringLiteralDoubleQuoteMultipleExpr
             , testStringLiteralUnfinishedMultilineFail
             , testStringLiteralUnfinishedMultilineFailExpr
             , testStringLiteralUnfinishedDoubleLineFail
             , testStringLiteralUnfinishedDoubleLineFailExpr
             , testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail
             , testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFailExpr
             , testStringLiteralUnfinishedFail
             , testStringLiteralUnfinishedFailExpr
             ]
