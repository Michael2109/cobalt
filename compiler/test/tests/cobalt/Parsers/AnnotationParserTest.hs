module Parsers.AnnotationParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

testAnnotationParserUpper :: Test
testAnnotationParserUpper = do
    let code = "@ANNOTATION"
    TestCase $ assertEqual code
        (Annotation "ANNOTATION")
        (case (parse (annotationParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testAnnotationParserLower :: Test
testAnnotationParserLower = do
    let code = "@annotation"
    TestCase $ assertEqual code
        (Annotation "annotation")
        (case (parse (annotationParser) "" code) of
             Left  _ -> Error
             Right x -> x)
