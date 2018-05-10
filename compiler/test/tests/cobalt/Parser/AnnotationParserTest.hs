module Parser.AnnotationParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testAnnotationParser :: Test
testAnnotationParser = do
    let code1 = "@ANNOTATION"
    let test1 = testParseSuccess code1 (Annotation $ Name "ANNOTATION") annotationParser

    let code2 = "@annotation"
    let test2 = testParseSuccess code2 (Annotation $ Name "annotation") annotationParser

    TestList [test1, test2]
