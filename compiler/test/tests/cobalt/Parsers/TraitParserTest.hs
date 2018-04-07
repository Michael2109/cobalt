module Parsers.TraitParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

testTraitParser :: Test
testTraitParser = do
    let code = unlines [ "trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameter :: Test
testTraitParserTypeParameter = do
    let code = unlines [ "trait Test[String]" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterExtends :: Test
testTraitParserTypeParameterExtends = do
    let code = unlines [ "trait Test[String] extends ParentName" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] (Just "ParentName") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterExtendsImplements :: Test
testTraitParserTypeParameterExtendsImplements = do
    let code = unlines [ "trait Test[String] extends ParentName implements TraitName" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] (Just "ParentName") ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterImplements :: Test
testTraitParserTypeParameterImplements = do
    let code = unlines [ "trait Test[String] implements TraitName" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterImplementsMultiple :: Test
testTraitParserTypeParameterImplementsMultiple = do
    let code = unlines [ "trait Test[String] implements TraitName1, TraitName2" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing ["TraitName1","TraitName2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserExtends :: Test
testTraitParserExtends = do
    let code = unlines [ "trait Test extends ParentClass" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] (Just "ParentClass") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserImplements :: Test
testTraitParserImplements = do
    let code = unlines [ "trait Test implements Interface" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] Nothing ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserImplementsMultiple :: Test
testTraitParserImplementsMultiple = do
    let code = unlines [ "trait Test implements Interface1, Interface2" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] Nothing ["Interface1","Interface2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserExtendsImplements :: Test
testTraitParserExtendsImplements = do
    let code = unlines [ "trait Test extends ParentClass implements Interface" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] (Just "ParentClass") ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserExtendsImplementsMultiple :: Test
testTraitParserExtendsImplementsMultiple = do
    let code = unlines [ "trait Test extends ParentClass implements Interface1, Interface2, Interface3" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] (Just "ParentClass") ["Interface1","Interface2","Interface3"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserImports :: Test
testTraitParserImports = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "trait Test extends ParentClass implements Interface"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] (Just "ParentClass") ["Interface"] imports [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserImportsFail :: Test
testTraitParserImportsFail = do
    let code = unlines [ "import -dir.sub_dir.ClassName"
                       , "trait Test extends ParentClass implements Interface"
                       ]
    TestCase $ assertEqual code
        (Error)
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserModifierBlock :: Test
testTraitParserModifierBlock = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "trait Test extends ParentClass implements Interface"
                       , "public"
                       , "  val x: int = 5"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    let modifierBlocks = [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [Argument $ ArithExpr (IntConst 5)]]]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] (Just "ParentClass") ["Interface"] imports modifierBlocks [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)
