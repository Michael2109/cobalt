module Parser.TupleParserTest where

import Test.HUnit


import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testTupleParser :: Test
testTupleParser = do
    let codeMultiple = "(x, y, z)"
    let testMultiple = testParseSuccess codeMultiple (Tuple (BlockExpr [Identifier (Name "x"),Identifier (Name "y"),Identifier (Name "z")])) expressionParser'

    let codeMultipleExpressions = "((1, \"String\"), y, 100, varName, new ClassName(a, b, c))"
    let testMultipleExpressions = testParseSuccess codeMultipleExpressions (Tuple (BlockExpr [Tuple (BlockExpr [IntConst 1,StringLiteral "String"]),Identifier (Name "y"),IntConst 100,Identifier (Name "varName"),NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [Identifier (Name "a"),Identifier (Name "b"),Identifier (Name "c")]) Nothing])) expressionParser'

    let codeTupleEmpty = "()"
    let testTupleEmpty = testParseFailure codeTupleEmpty expressionParser'

    let codeSingle = "(a)"
    let testSingle = testParseSuccess codeSingle (Identifier $ Name "a") expressionParser'

    TestList [ testMultiple
             , testMultipleExpressions
             , testTupleEmpty
             , testSingle
             ]
