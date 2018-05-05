module Parser.ImportParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testImportParser :: Test
testImportParser = do
    let code = "import x"
    let test = testParseSuccess code (Import ["x"]) importParser

    let codeTwo = "import x.y"
    let testTwo = testParseSuccess codeTwo (Import ["x", "y"]) importParser

    let codeMultiple = "import x.y.z.a.b.c"
    let testMultiple = testParseSuccess codeMultiple (Import ["x", "y", "z", "a", "b", "c"]) importParser

    let codeCapital = "import abc.xyz.Name"
    let testCapital = testParseSuccess codeCapital (Import ["abc", "xyz", "Name"]) importParser

    let codeUnderscore = "import _abc.xy_z.Name_"
    let testUnderscore = testParseSuccess codeUnderscore (Import ["_abc", "xy_z", "Name_"]) importParser

    let codeContainsDigits = "import a1b2c3.x1y2z3"
    let testContainsDigits = testParseSuccess codeContainsDigits (Import ["a1b2c3", "x1y2z3"]) importParser

    TestList [ test
             , testTwo
             , testMultiple
             , testCapital
             , testUnderscore
             , testContainsDigits
             ]
