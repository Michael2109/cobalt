module Parser.TupleParserTest where

import Test.HUnit


import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testTupleParser :: Test
testTupleParser = do
    let codeMultiple = "(x, y, z)"
    let testMultiple = testParseSuccess codeMultiple (Tuple (BlockExpr [Identifier (Name "x"),Identifier (Name "y"),Identifier (Name "z")])) expressionParser'

    let codeTupleEmpty = "()"
    let testTupleEmpty = testParseFailure codeTupleEmpty expressionParser'

    -- let codeSingle = "(a)"
    -- let testSingle = testParseFailure codeSingle expressionParser'

    TestList [ testMultiple
             , testTupleEmpty
             --, testSingle
             ]
