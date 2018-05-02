module Parser.ParserTests where

import Test.HUnit

import Parser.BaseParserTest
import Parser.AExprParserTest
import Parser.BExprParserTest
import Parser.AnnotationParserTest
import Parser.AssignParserTest
import Parser.DoBlockParserTest
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
import Parser.ThisVarParserTest
import Parser.TupleParserTest
import Parser.TypeParameterParserTest
import Parser.ValueTypeParserTest

parserTestList :: Test
parserTestList = TestList

    [ testSymbol
    , testReservedWord
    , testIdentifier
    , testFloat
    , testDouble
    , testInteger
    , testLong

    , testAExprParser

    , testBExprParser

    , testAnnotationParser

    , testAssignParser
    , testAssignParserMultiple

    , testExpressionParserNested

    , testForLoopGeneratorParser

    , testIdentifierParserOneCharacter

    , testIdentifierParserContainsUnderscore

    , testIdentifierParserContainsDigit

    , testIdentifierParserCapital

    , testIfStmtParser

    , testLambdaParser

    , testMethodCallParser
    , testMethodCallParserExpr

    , testMethodParser

    , testModelParser

    , testModelParserInner

    , testNewClassInstanceParser
    , testNewClassInstanceParserExpr

    , testRExprParser

    , testTernaryParser

    , testTupleParser
    ]
