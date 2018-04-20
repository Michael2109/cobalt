module Parser.AnnotationParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testAnnotationParser :: Test
testAnnotationParser = do
    let code1 = "@ANNOTATION"
    let test1 = TestCase $ assertEqual code1
                    (Annotation $ Name "ANNOTATION")
                    (case (parse (annotationParser) "" code1) of
                         Left  e -> error $ show e
                         Right x -> x)
    let code2 = "@annotation"
    let test2 = TestCase $ assertEqual code2
                    (Annotation $ Name "annotation")
                    (case (parse (annotationParser) "" code2) of
                         Left  e -> error $ show e
                         Right x -> x)
    TestList [test1, test2]
