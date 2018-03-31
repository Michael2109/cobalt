module IfElseStatementParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testIfStmtParserBooleanTrue :: Test
testIfStmtParserBooleanTrue = do
  let code = unlines [
        "if(True)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument $ BooleanExpr (BoolConst True)) [Print (Argument $ StringLiteral "Test")])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

testIfStmtParserBooleanFalse :: Test
testIfStmtParserBooleanFalse = do
  let code = unlines [
        "if(True)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument $ BooleanExpr (BoolConst True)) [Print (Argument $ StringLiteral "Test")])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

testIfStmtParserObjectVar :: Test
testIfStmtParserObjectVar = do
  let code = unlines [
        "if(True and objName.varName)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument $ BooleanExpr (BBinary And (BoolConst True) (ClassVariable "objName" "varName"))) [Print (Argument $ StringLiteral "Test")])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

testIfStmtParserAnd :: Test
testIfStmtParserAnd = do
  let code = unlines [
        "if(True and objName.varName and varName)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument (BooleanExpr (BBinary And (BBinary And (BoolConst True) (ClassVariable "objName" "varName"))(Identifier "varName")))) [Print (Argument (StringLiteral "Test"))])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

testIfStmtParserOr :: Test
testIfStmtParserOr = do
  let code = unlines [
        "if(True or objName.varName)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument (BooleanExpr (BBinary Or (BoolConst True) (ClassVariable "objName" "varName")))) [Print (Argument (StringLiteral "Test"))])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

testIfStmtParserAndOr :: Test
testIfStmtParserAndOr = do
  let code = unlines [
        "if(True and varName or objName.varName)",
        "  println(\"Test\")"
        ]
  TestCase $ assertEqual code
    (If (Argument (BooleanExpr (BBinary Or (BBinary And (BoolConst True) (Identifier "varName")) (ClassVariable "objName" "varName")))) [Print (Argument (StringLiteral "Test"))])
    (case (parse (ifStmtParser) "" code) of
      Left  e -> Error
      Right x -> x)

