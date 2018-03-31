module AnnotationParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testAnnotationParserUpper :: Test
testAnnotationParserUpper = do
  let code = "@ANNOTATION"
  TestCase $ assertEqual code
    (Annotation "ANNOTATION")
    (case (parse (annotationParser) "" code) of
      Left  e -> Error
      Right x -> x)

testAnnotationParserLower :: Test
testAnnotationParserLower = do
  let code = "@annotation"
  TestCase $ assertEqual code
    (Annotation "annotation")
    (case (parse (annotationParser) "" code) of
      Left  e -> Error
      Right x -> x)