module Parser.ParserTests where

import Test.HUnit

import Parser.AExprParserTest
import Parser.BaseParserTest
import Parser.BExprParserTest
import Parser.AnnotationParserTest
import Parser.AssignParserTest
import Parser.ExpressionParserTest
import Parser.ForLoopParserTest
import Parser.IdentifierParserTest
import Parser.IfStatementParserTest
import Parser.ImportParserTest
import Parser.LambdaParserTest
import Parser.MethodCallParserTest
import Parser.MethodParserTest
import Parser.ModelTypeParserTest
import Parser.ModifierParserTest
import Parser.ModelParserTest
import Parser.ModifierBlockParserTest
import Parser.NewClassInstanceParserTest
import Parser.PackageParserTest
import Parser.ParameterizedTypeParserTest
import Parser.ParameterParserTest
import Parser.ReassignParserTest
import Parser.StringLiteralParserTest
import Parser.TernaryParserTest
import Parser.TupleParserTest
import Parser.TypeParameterParserTest

parserTestList :: Test
parserTestList = TestList

    [ testSymbol
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
    , testMethodCallParser
    , testMethodParser
    , testModelParser
    , testModifierParser
    , testNewClassInstanceParser
    , testRParser
    , testTernaryParser
    , testTupleParser
    ]
