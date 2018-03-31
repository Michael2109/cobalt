module ClassVariableParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor


testClassVariableParser :: Test
testClassVariableParser = do
  let code = "objName.varName"
  TestCase $ assertEqual code
    (ClassVariable "objName" "varName")
    (case (parse (classVariableParser) "" code) of
      Left  e -> Error
      Right x -> x)

testClassVariableParserUnderscores :: Test
testClassVariableParserUnderscores = do
  let code = "obj_name.var_name"
  TestCase $ assertEqual code
    (ClassVariable "obj_name" "var_name")
    (case (parse (classVariableParser) "" code) of
      Left  e -> Error
      Right x -> x)

testClassVariableParserStartCapitals :: Test
testClassVariableParserStartCapitals = do
  let code = "Obj_name.Var_name"
  TestCase $ assertEqual code
    (ClassVariable "Obj_name" "Var_name")
    (case (parse (classVariableParser) "" code) of
      Left  e -> Error
      Right x -> x)

testClassVariableParserMissingVar :: Test
testClassVariableParserMissingVar = do
  let code = "obj_name."
  TestCase $ assertEqual code
    Error
    (case (parse (classVariableParser) "" code) of
      Left  e -> Error
      Right x -> x)

testClassVariableParserMissingClassName :: Test
testClassVariableParserMissingClassName = do
  let code = ".varName"
  TestCase $ assertEqual code
    Error
    (case (parse (classVariableParser) "" code) of
      Left  e -> Error
      Right x -> x)