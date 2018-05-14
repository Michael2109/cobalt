module Parser.NameSpaceParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testNameSpaceParser :: Test
testNameSpaceParser = do
    let code = "package dir.sub_dir"
    testParseSuccess code (NameSpace ["dir","sub_dir"]) nameSpaceParser
