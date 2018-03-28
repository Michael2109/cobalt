module ObjectParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser


testObjectParser :: Test
testObjectParser = do
  let code = unlines [
        "object Test"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] Nothing [] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserTypeParameter :: Test
testObjectParserTypeParameter = do
  let code = unlines [
        "object Test[String]"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing [] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserTypeParameterExtends :: Test
testObjectParserTypeParameterExtends = do
  let code = unlines [
        "object Test[String] extends ParentName"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" (Just (TypeParameter (Identifier "String"))) [] (Just "ParentName") [] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserTypeParameterExtendsImplements :: Test
testObjectParserTypeParameterExtendsImplements = do
  let code = unlines [
        "object Test[String] extends ParentName implements TraitName"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" (Just (TypeParameter (Identifier "String"))) [] (Just "ParentName") ["TraitName"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserTypeParameterImplements :: Test
testObjectParserTypeParameterImplements = do
  let code = unlines [
        "object Test[String] implements TraitName"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing ["TraitName"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserTypeParameterImplementsMultiple :: Test
testObjectParserTypeParameterImplementsMultiple = do
  let code = unlines [
        "object Test[String] implements TraitName1, TraitName2"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing ["TraitName1","TraitName2"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserExtends :: Test
testObjectParserExtends = do
  let code = unlines [
        "object Test extends ParentObject"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] (Just "ParentObject") [] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)


testObjectParserImplements :: Test
testObjectParserImplements = do
  let code = unlines [
        "object Test implements Interface"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] Nothing ["Interface"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserImplementsMultiple :: Test
testObjectParserImplementsMultiple = do
  let code = unlines [
        "object Test implements Interface1, Interface2"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] Nothing ["Interface1","Interface2"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserExtendsImplements :: Test
testObjectParserExtendsImplements = do
  let code = unlines [
        "object Test extends ParentObject implements Interface"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] (Just "ParentObject") ["Interface"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserExtendsImplementsMultiple :: Test
testObjectParserExtendsImplementsMultiple = do
  let code = unlines [
        "object Test extends ParentObject implements Interface1, Interface2, Interface3"
        ]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] (Just "ParentObject") ["Interface1","Interface2","Interface3"] [] [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserImports :: Test
testObjectParserImports = do
  let code = unlines [
        "import dir.sub_dir.ObjectName",
        "object Test extends ParentObject implements Interface"
        ]
  let imports = [Import ["dir", "sub_dir", "ObjectName"]]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] (Just "ParentObject") ["Interface"] imports [] [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserImportsFail :: Test
testObjectParserImportsFail = do
  let code = unlines [
        "import -dir.sub_dir.ObjectName",
        "object Test extends ParentObject implements Interface"
        ]
  TestCase $ assertEqual code
    (Error)
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testObjectParserModifierBlock :: Test
testObjectParserModifierBlock = do
  let code = unlines [
        "import dir.sub_dir.ObjectName",
        "object Test extends ParentObject implements Interface",
        "public",
        "  val x: int = 5"

        ]
  let imports = [Import ["dir", "sub_dir", "ObjectName"]]
  let modifierBlocks = [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [Argument $ ArithExpr (IntConst 5)]]]
  TestCase $ assertEqual code
    (Object [] "Test" Nothing [] (Just "ParentObject") ["Interface"] imports modifierBlocks [] [])
    (case (parse (objectParser []) "" code) of
      Left  e -> Error
      Right x -> x)
