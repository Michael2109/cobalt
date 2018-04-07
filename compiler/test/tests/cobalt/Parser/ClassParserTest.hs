module Parser.ClassParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testClassParser :: Test
testClassParser = do
    let code = unlines [ "class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameter :: Test
testClassParserTypeParameter = do
    let code = unlines [ "class Test[String]" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterExtends :: Test
testClassParserTypeParameterExtends = do
    let code = unlines [ "class Test[String] extends ParentName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] (Just "ParentName") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterExtendsImplements :: Test
testClassParserTypeParameterExtendsImplements = do
    let code = unlines [ "class Test[String] extends ParentName implements TraitName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] (Just "ParentName") ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterImplements :: Test
testClassParserTypeParameterImplements = do
    let code = unlines [ "class Test[String] implements TraitName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterImplementsMultiple :: Test
testClassParserTypeParameterImplementsMultiple = do
    let code = unlines [ "class Test[String] implements TraitName1, TraitName2" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] Nothing ["TraitName1","TraitName2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserExtends :: Test
testClassParserExtends = do
    let code = unlines [ "class Test extends ParentClass" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] (Just "ParentClass") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImplements :: Test
testClassParserImplements = do
    let code = unlines [ "class Test implements Interface" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] Nothing ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImplementsMultiple :: Test
testClassParserImplementsMultiple = do
    let code = unlines [ "class Test implements Interface1, Interface2" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] Nothing ["Interface1","Interface2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserExtendsImplements :: Test
testClassParserExtendsImplements = do
    let code = unlines [ "class Test extends ParentClass implements Interface" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] (Just "ParentClass") ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserExtendsImplementsMultiple :: Test
testClassParserExtendsImplementsMultiple = do
    let code = unlines [ "class Test extends ParentClass implements Interface1, Interface2, Interface3" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] (Just "ParentClass") ["Interface1","Interface2","Interface3"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImports :: Test
testClassParserImports = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] (Just "ParentClass") ["Interface"] imports [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImportsFail :: Test
testClassParserImportsFail = do
    let code = unlines [ "import -dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       ]
    TestCase $ assertEqual code
        (Error)
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserModifierBlock :: Test
testClassParserModifierBlock = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "  val x: int = 5"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    let modifierBlocks = [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [Argument $ ArithExpr (IntConst 5)]]]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] (Just "ParentClass") ["Interface"] imports modifierBlocks [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)
