module Parser.ParserTests where

import Test.HUnit

import Parser.AExprParserTest
import Parser.ArrayOpParserTest
import Parser.BaseParserTest
import Parser.BExprParserTest
import Parser.AnnotationParserTest
import Parser.AssignParserTest
import Parser.ExpressionParserTest
import Parser.ForLoopParserTest
import Parser.IdentifierParserTest
import Parser.IfStatementParserTest
import Parser.LambdaParserTest
import Parser.MatchParserTest
import Parser.MethodCallParserTest
import Parser.MethodParserTest
import Parser.ModifierParserTest
import Parser.ModelParserTest
import Parser.NameSpaceParserTest
import Parser.NewClassInstanceParserTest
import Parser.PrintParserTest
import Parser.ReassignParserTest
import Parser.StringLiteralParserTest
import Parser.TernaryParserTest
import Parser.TupleParserTest

parserTestList :: Test
parserTestList = TestList

    [ testArrayOpParser
    , testSymbol
    , testReservedWord
    , testIdentifier
    , testFloat
    , testDouble
    , testInteger
    , testLong
    , testAParser
    , testBParser
    , testAnnotationParser
    , testAssignParser
    , testAssignParserMultiple
    , testExpressionParserNested
    , testForLoopGeneratorParser
    , testIdentifierParser
    , testIfStmtParser
    , testLambdaParser
    , testMatchParser
    , testMethodCallParser
    , testMethodParser
    , testModelParser
    , testModifierParser
    , testNameSpaceParser
    , testNewClassInstanceParser
    , testPrintParser
    , testRParser
    , testReassignParser
    , testStringLiteralParser
    , testTernaryParser
    , testTupleParser
    ]
