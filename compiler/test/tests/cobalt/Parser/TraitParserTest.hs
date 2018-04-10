module Parser.TraitParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import AST.Data.Modifier
import Parser.ExprParser

testTraitParser :: Test
testTraitParser = do
    let code = unlines [ "trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameter :: Test
testTraitParserTypeParameter = do
    let code = unlines [ "trait Test[String]" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterExtends :: Test
testTraitParserTypeParameterExtends = do
    let code = unlines [ "trait Test[String] extends ParentName" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterExtendsImplements :: Test
testTraitParserTypeParameterExtendsImplements = do
    let code = unlines [ "trait Test[String] extends ParentName implements TraitName" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterImplements :: Test
testTraitParserTypeParameterImplements = do
    let code = unlines [ "trait Test[String] implements TraitName" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserTypeParameterImplementsMultiple :: Test
testTraitParserTypeParameterImplementsMultiple = do
    let code = unlines [ "trait Test[String] implements TraitName1, TraitName2" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing ["TraitName1","TraitName2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserExtends :: Test
testTraitParserExtends = do
    let code = unlines [ "trait Test extends ParentClass" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] (Just "ParentClass") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserImplements :: Test
testTraitParserImplements = do
    let code = unlines [ "trait Test implements Interface" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] Nothing ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserImplementsMultiple :: Test
testTraitParserImplementsMultiple = do
    let code = unlines [ "trait Test implements Interface1, Interface2" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] Nothing ["Interface1","Interface2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserExtendsImplements :: Test
testTraitParserExtendsImplements = do
    let code = unlines [ "trait Test extends ParentClass implements Interface" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserExtendsImplementsMultiple :: Test
testTraitParserExtendsImplementsMultiple = do
    let code = unlines [ "trait Test extends ParentClass implements Interface1, Interface2, Interface3" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface1","Interface2","Interface3"] [] [] [] [])
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
        (Trait Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] imports [] [] [])
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
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] [Import ["dir","sub_dir","ClassName"]] [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [IntConst 5]]] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Modifiers
testTraitParserPublic :: Test
testTraitParserPublic = do
    let code = unlines [ "public trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserProtected :: Test
testTraitParserProtected = do
    let code = unlines [ "protected trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Protected] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserPrivate :: Test
testTraitParserPrivate = do
    let code = unlines [ "private trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Private] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserPublicAbstract :: Test
testTraitParserPublicAbstract = do
    let code = unlines [ "public abstract trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Public, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserProtectedAbstract :: Test
testTraitParserProtectedAbstract = do
    let code = unlines [ "protected abstract trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Protected, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserPrivateAbstract :: Test
testTraitParserPrivateAbstract = do
    let code = unlines [ "private abstract trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Private, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserAbstract :: Test
testTraitParserAbstract = do
    let code = unlines [ "abstract trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserPublicFinal :: Test
testTraitParserPublicFinal = do
    let code = unlines [ "public final trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Public, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserProtectedFinal :: Test
testTraitParserProtectedFinal = do
    let code = unlines [ "protected final trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Protected, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserPrivateFinal :: Test
testTraitParserPrivateFinal = do
    let code = unlines [ "private final trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Private, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserFinal :: Test
testTraitParserFinal = do
    let code = unlines [ "final trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserReordered1 :: Test
testTraitParserReordered1 = do
    let code = unlines [ "abstract public trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Abstract, Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserReordered2 :: Test
testTraitParserReordered2 = do
    let code = unlines [ "final abstract public trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Final, Abstract, Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTraitParserReordered3 :: Test
testTraitParserReordered3 = do
    let code = unlines [ "abstract private final trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [Abstract, Private, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)