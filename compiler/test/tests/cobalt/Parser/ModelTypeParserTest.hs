module Parser.ModelTypeParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testModelTypeParserClass :: Test
testModelTypeParserClass = do
    let code = "class"
    TestCase $ assertEqual code
        ClassModel
        (case (parse modelTypeParser "" code) of
             Left  _ -> error "Model type incorrectly parsed"
             Right x -> x)

testModelTypeParserObject :: Test
testModelTypeParserObject = do
    let code = "object"
    TestCase $ assertEqual code
        ObjectModel
        (case (parse modelTypeParser "" code) of
             Left  _ -> error "Model type incorrectly parsed"
             Right x -> x)

testModelTypeParserTrait :: Test
testModelTypeParserTrait = do
    let code = "trait"
    TestCase $ assertEqual code
        TraitModel
        (case (parse modelTypeParser "" code) of
             Left  _ -> error "Model type incorrectly parsed"
             Right x -> x)
