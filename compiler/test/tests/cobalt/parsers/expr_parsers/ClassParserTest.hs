module ClassParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser


testClassParser :: Test
testClassParser = do
  let code = unlines [
        "class Test"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] Nothing [] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserTypeParameter :: Test
testClassParserTypeParameter = do
  let code = unlines [
        "class Test[String]"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing [] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserExtends :: Test
testClassParserExtends = do
  let code = unlines [
        "class Test extends ParentClass"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] (Just "ParentClass") [] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)


testClassParserImplements :: Test
testClassParserImplements = do
  let code = unlines [
        "class Test implements Interface"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] Nothing ["Interface"] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserImplementsMultiple :: Test
testClassParserImplementsMultiple = do
  let code = unlines [
        "class Test implements Interface1, Interface2"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] Nothing ["Interface1","Interface2"] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserExtendsImplements :: Test
testClassParserExtendsImplements = do
  let code = unlines [
        "class Test extends ParentClass implements Interface"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] (Just "ParentClass") ["Interface"] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserExtendsImplementsMultiple :: Test
testClassParserExtendsImplementsMultiple = do
  let code = unlines [
        "class Test extends ParentClass implements Interface1, Interface2, Interface3"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] (Just "ParentClass") ["Interface1","Interface2","Interface3"] [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserImports :: Test
testClassParserImports = do
  let code = unlines [
        "import dir.sub_dir.ClassName",
        "class Test extends ParentClass implements Interface"
        ]
  let imports = [Import ["dir", "sub_dir", "ClassName"]]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] (Just "ParentClass") ["Interface"] imports [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserImportsFail :: Test
testClassParserImportsFail = do
  let code = unlines [
        "import -dir.sub_dir.ClassName",
        "class Test extends ParentClass implements Interface"
        ]
  TestCase $ assertEqual code
    (Error)
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserModifierBlock :: Test
testClassParserModifierBlock = do
  let code = unlines [
        "import dir.sub_dir.ClassName",
        "class Test extends ParentClass implements Interface",
        "public",
        "  val x: int = 5"

        ]
  let imports = [Import ["dir", "sub_dir", "ClassName"]]
  let modifierBlocks = [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [Argument $ ArithExpr (IntConst 5)]]]
  TestCase $ assertEqual code
    (Class [] "Test" Nothing [] (Just "ParentClass") ["Interface"] imports modifierBlocks [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)
