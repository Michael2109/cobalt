module ModifierBlockParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testModifierBlockParserPrivate :: Test
testModifierBlockParserPrivate = do
  let code = unlines [
        "private",
        "  val x: int = 5",
        "  val y: int = 10"
        ]

  TestCase $ assertEqual code
    (ModifierBlock [GlobalVar "private" True True (Type (Identifier "int")) (Identifier "x") [Argument (ArithExpr (IntConst 5))],GlobalVar "private" True True (Type (Identifier "int")) (Identifier "y") [Argument (ArithExpr (IntConst 10))]])
    (case (parse (modifierBlockParser True) "" code) of
      Left  e -> Error
      Right x -> x)

testModifierBlockParserProtected :: Test
testModifierBlockParserProtected = do
  let code = unlines [
        "protected",
        "  val x: int = 5",
        "  val y: int = 10"
        ]

  TestCase $ assertEqual code
    (ModifierBlock [GlobalVar "protected" True True (Type (Identifier "int")) (Identifier "x") [Argument (ArithExpr (IntConst 5))],GlobalVar "protected" True True (Type (Identifier "int")) (Identifier "y") [Argument (ArithExpr (IntConst 10))]])
    (case (parse (modifierBlockParser True) "" code) of
      Left  e -> Error
      Right x -> x)



testModifierBlockParserPublic :: Test
testModifierBlockParserPublic = do
  let code = unlines [
        "public",
        "  val x: int = 5",
        "  val y: int = 10"
        ]

  TestCase $ assertEqual code
    (ModifierBlock [GlobalVar "public" True True (Type (Identifier "int")) (Identifier "x") [Argument (ArithExpr (IntConst 5))],GlobalVar "public" True True (Type (Identifier "int")) (Identifier "y") [Argument (ArithExpr (IntConst 10))]])
    (case (parse (modifierBlockParser True) "" code) of
      Left  e -> Error
      Right x -> x)

testModifierBlockParserPrivateEmpty :: Test
testModifierBlockParserPrivateEmpty = do
  let code = unlines [
        "public"
        ]

  TestCase $ assertEqual code
    (ModifierBlock [])
    (case (parse (modifierBlockParser True) "" code) of
      Left  e -> Error
      Right x -> x)

testModifierBlockParserProtectedEmpty :: Test
testModifierBlockParserProtectedEmpty = do
  let code = unlines [
        "public"
        ]

  TestCase $ assertEqual code
    (ModifierBlock [])
    (case (parse (modifierBlockParser True) "" code) of
      Left  e -> Error
      Right x -> x)

testModifierBlockParserPublicEmpty :: Test
testModifierBlockParserPublicEmpty = do
  let code = unlines [
        "public"
        ]

  TestCase $ assertEqual code
    (ModifierBlock [])
    (case (parse (modifierBlockParser True) "" code) of
      Left  e -> Error
      Right x -> x)