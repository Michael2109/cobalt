module Parser.TernaryParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testTernaryParser :: Test
testTernaryParser = do

    let codeTernaryParenthesis = "if (True) then i else j"
    let testTernaryParenthesis = testParseSuccess codeTernaryParenthesis (Ternary (BoolConst True) (Identifier (Name "i")) (Identifier (Name "j"))) ternaryParser

    let codeTernaryNoParenthesis = "if True then i else j"
    let testTernaryNoParenthesis = testParseSuccess codeTernaryNoParenthesis (Ternary (BoolConst True) (Identifier (Name "i")) (Identifier (Name "j"))) ternaryParser

    TestList [testTernaryParenthesis, testTernaryNoParenthesis]
