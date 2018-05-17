module Parser.IdentifierParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testIdentifierParser :: Test
testIdentifierParser = do
    let code1 = "x"
    let test1 = testParseSuccess code1 (Identifier $ Name "x") expressionParser'

    let codeContainsUnderscores = "_an_identifier"
    let testContainsUnderscores = testParseSuccess codeContainsUnderscores (Identifier (Name "_an_identifier")) expressionParser'

    let codeContainsDigits = "a1b2c3"
    let testContainsDigits = testParseSuccess codeContainsDigits (Identifier $ Name "a1b2c3") expressionParser'

    let codeCapital = "ID"
    let testCapital = testParseSuccess codeCapital (Identifier $ Name "ID") expressionParser'

    TestList [ test1
             , testContainsUnderscores
             , testContainsDigits
             , testCapital
             ]
