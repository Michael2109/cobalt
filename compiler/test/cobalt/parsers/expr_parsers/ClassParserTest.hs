module ClassParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

--Class [String]    String (Maybe [Expr]) (Maybe String) (Maybe String) [Expr]   [Expr]         [Expr]           [Expr]
--Class packageLocs name   params         parent         interfaces     imports  modifierBlocks constructorExprs bodyArray

testClassParser :: Test
testClassParser = do
  let code = unlines [
        "class Test"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" (Nothing) Nothing Nothing [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)

testClassParserExtends :: Test
testClassParserExtends = do
  let code = unlines [
        "class Test extends ParentClass"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" (Nothing) (Just "ParentClass") Nothing [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)


testClassParserImplements :: Test
testClassParserImplements = do
  let code = unlines [
        "class Test implements Interface"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" (Nothing) Nothing (Just "Interface") [] [] [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)


testClassParserExtendsImplements :: Test
testClassParserExtendsImplements = do
  let code = unlines [
        "class Test extends ParentClass implements Interface"
        ]
  TestCase $ assertEqual code
    (Class [] "Test" (Nothing) (Just "ParentClass") (Just "Interface") [] [] [] [])
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
    (Class [] "Test" (Nothing) (Just "ParentClass") (Just "Interface") imports [] [] [])
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

--GlobalVar modifier final static varType varName exprs

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
    (Class [] "Test" (Nothing) (Just "ParentClass") (Just "Interface") imports modifierBlocks [] [])
    (case (parse (classParser []) "" code) of
      Left  e -> Error
      Right x -> x)