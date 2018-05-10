module Parser.TupleParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testTupleParser :: Test
testTupleParser = do
    let codeTuple = "(x, y, z)"
    let testTuple = testParseSuccess codeTuple (Tuple (BlockExpr [Identifier (Name "x"),Identifier (Name "y"),Identifier (Name "z")])) tupleParser

    TestList [testTuple]
