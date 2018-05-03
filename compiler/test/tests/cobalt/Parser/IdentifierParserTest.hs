module Parser.IdentifierParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testIdentifierParser :: Test
testIdentifierParser = do
    let code = "x"
    let test = testParseSuccess code (Identifier $ Name "x") identifierParser
    let testExpr = testParseSuccess code (Identifier $ Name "x") expressionParser'

    let codeContainsUnderscores = "_an_identifier"
    let testContainsUnderscores = testParseSuccess codeContainsUnderscores (Identifier (Name "_an_identifier")) identifierParser
    let testContainsUnderscoresExpr = testParseSuccess codeContainsUnderscores (Identifier (Name "_an_identifier")) expressionParser'

    let codeContainsDigits = "a1b2c3"
    let testContainsDigits = testParseSuccess codeContainsDigits (Identifier $ Name "a1b2c3") identifierParser
    let testContainsDigitsExpr = testParseSuccess codeContainsDigits (Identifier $ Name "a1b2c3") expressionParser'

    let codeCapital = "ID"
    let testCapital = testParseSuccess codeCapital (Identifier $ Name "ID") identifierParser
    let testCapitalExpr = testParseSuccess codeCapital (Identifier $ Name "ID") expressionParser'

    TestList [ test
             , testExpr
             , testContainsUnderscores
             , testContainsUnderscoresExpr
             , testContainsDigits
             , testContainsDigitsExpr
             , testCapital
             , testCapitalExpr
             ]
