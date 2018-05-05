module Parser.ModelTypeParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testModelTypeParser :: Test
testModelTypeParser = do
    let codeClass = "class"
    let testClass = testParseSuccess codeClass ClassModel modelTypeParser

    let codeObject = "object"
    let testObject = testParseSuccess codeObject ObjectModel modelTypeParser

    let codeTrait = "trait"
    let testTrait = testParseSuccess codeClass TraitModel modelTypeParser

    TestList [ testClass
             , testObject
             , testTrait
             ]
