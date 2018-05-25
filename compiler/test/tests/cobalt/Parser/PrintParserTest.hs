module Parser.PrintParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testPrintParser :: Test
testPrintParser = do

    let codePrintSingleArgument = "print(a)"
    let testPrintSingleArgument = testParseSuccess codePrintSingleArgument (Print (Identifier (Name "a"))) statementParser

    let codePrintlnSingleArgument = "print(a)"
    let testPrintlnSingleArgument = testParseSuccess codePrintlnSingleArgument (Print (Identifier (Name "a"))) statementParser

    TestList [ testPrintSingleArgument
             , testPrintlnSingleArgument
             ]
