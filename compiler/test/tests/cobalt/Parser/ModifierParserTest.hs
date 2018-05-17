module Parser.ModifierParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testModifierParser :: Test
testModifierParser = do
    let codePublic = "public"
    let testPublic = testParseSuccess codePublic [Public] modifiersParser

    let codeProtected = "protected"
    let testProtected = testParseSuccess codeProtected [Protected] modifiersParser

    let codeLocal = "local"
    let testLocal = testParseSuccess codeLocal [PackageLocal] modifiersParser

    let codeAbstract = "abstract"
    let testAbstract = testParseSuccess codeAbstract [Abstract] modifiersParser

    let codeFinal = "final"
    let testFinal = testParseSuccess codeFinal [Final] modifiersParser

    TestList [ testPublic
             , testProtected
             , testLocal
             , testAbstract
             , testFinal
             ]
