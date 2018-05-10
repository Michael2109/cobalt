module Parser.TupleParserTest where

import Test.HUnit


import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testTupleParser :: Test
testTupleParser = do
    let code1 = "(x, y, z)"
    let test1 = testParseSuccess code1 (Tuple (BlockExpr [Identifier (Name "x"),Identifier (Name "y"),Identifier (Name "z")])) expressionParser'

    TestList [test1]
