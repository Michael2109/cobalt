module Parser.ModifierParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testModifierParser :: Test
testModifierParser = do
    let codePublic = "public"
    let testPublic = testParseSuccess codePublic [Public] modifiersParser

    let codeProtected = "protected"
    let testProtected = testParseSuccess codeProtected [Protected] modifiersParser

    let codePrivate = "private"
    let testPrivate = testParseSuccess codePrivate [Private] modifiersParser

    let codeAbstract = "abstract"
    let testAbstract = testParseSuccess codeAbstract [Abstract] modifiersParser

    let codeFinal = "final"
    let testFinal = testParseSuccess codeFinal [Final] modifiersParser

    TestList [ testPublic
             , testProtected
             , testPrivate
             , testAbstract
             , testFinal
             ]
