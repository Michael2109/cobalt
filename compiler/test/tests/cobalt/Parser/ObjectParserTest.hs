module Parser.ObjectParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import AST.Data.Modifier
import Parser.ExprParser

testObjectParser :: Test
testObjectParser = do
    let code = unlines [ "object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserTypeParameter :: Test
testObjectParserTypeParameter = do
    let code = unlines [ "object Test[String]" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserTypeParameterExtends :: Test
testObjectParserTypeParameterExtends = do
    let code = unlines [ "object Test[String] extends ParentName" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserTypeParameterExtendsImplements :: Test
testObjectParserTypeParameterExtendsImplements = do
    let code = unlines [ "object Test[String] extends ParentName implements TraitName" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserTypeParameterImplements :: Test
testObjectParserTypeParameterImplements = do
    let code = unlines [ "object Test[String] implements TraitName" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserTypeParameterImplementsMultiple :: Test
testObjectParserTypeParameterImplementsMultiple = do
    let code = unlines [ "object Test[String] implements TraitName1, TraitName2" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing ["TraitName1","TraitName2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserExtends :: Test
testObjectParserExtends = do
    let code = unlines [ "object Test extends ParentObject" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] (Just "ParentObject") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserImplements :: Test
testObjectParserImplements = do
    let code = unlines [ "object Test implements Interface" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] Nothing ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserImplementsMultiple :: Test
testObjectParserImplementsMultiple = do
    let code = unlines [ "object Test implements Interface1, Interface2" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] Nothing ["Interface1","Interface2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserExtendsImplements :: Test
testObjectParserExtendsImplements = do
    let code = unlines [ "object Test extends ParentObject implements Interface" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] (Just "ParentObject") ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserExtendsImplementsMultiple :: Test
testObjectParserExtendsImplementsMultiple = do
    let code = unlines [ "object Test extends ParentObject implements Interface1, Interface2, Interface3" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] (Just "ParentObject") ["Interface1","Interface2","Interface3"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserImports :: Test
testObjectParserImports = do
    let code = unlines [ "import dir.sub_dir.ObjectName"
                       , "object Test extends ParentObject implements Interface"
                       ]
    let imports = [Import ["dir", "sub_dir", "ObjectName"]]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] (Just "ParentObject") ["Interface"] imports [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserImportsFail :: Test
testObjectParserImportsFail = do
    let code = unlines [ "import -dir.sub_dir.ObjectName"
                       , "object Test extends ParentObject implements Interface"
                       ]
    TestCase $ assertEqual code
        (Error)
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserModifierBlock :: Test
testObjectParserModifierBlock = do
    let code = unlines [ "import dir.sub_dir.ObjectName"
                       , "object Test extends ParentObject implements Interface"
                       , "public"
                       , "  val x: int = 5"
                       ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] (Just "ParentObject") ["Interface"] [Import ["dir","sub_dir","ObjectName"]] [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [IntConst 5]]] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Modifiers
testObjectParserPublic :: Test
testObjectParserPublic = do
    let code = unlines [ "public object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserProtected :: Test
testObjectParserProtected = do
    let code = unlines [ "protected object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Protected] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserPrivate :: Test
testObjectParserPrivate = do
    let code = unlines [ "private object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Private] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserPublicAbstract :: Test
testObjectParserPublicAbstract = do
    let code = unlines [ "public abstract object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Public, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserProtectedAbstract :: Test
testObjectParserProtectedAbstract = do
    let code = unlines [ "protected abstract object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Protected, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserPrivateAbstract :: Test
testObjectParserPrivateAbstract = do
    let code = unlines [ "private abstract object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Private, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserAbstract :: Test
testObjectParserAbstract = do
    let code = unlines [ "abstract object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserPublicFinal :: Test
testObjectParserPublicFinal = do
    let code = unlines [ "public final object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Public, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserProtectedFinal :: Test
testObjectParserProtectedFinal = do
    let code = unlines [ "protected final object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Protected, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserPrivateFinal :: Test
testObjectParserPrivateFinal = do
    let code = unlines [ "private final object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Private, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserFinal :: Test
testObjectParserFinal = do
    let code = unlines [ "final object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserReordered1 :: Test
testObjectParserReordered1 = do
    let code = unlines [ "abstract public object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Abstract, Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserReordered2 :: Test
testObjectParserReordered2 = do
    let code = unlines [ "final abstract public object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Final, Abstract, Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectParserReordered3 :: Test
testObjectParserReordered3 = do
    let code = unlines [ "abstract private final object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [Abstract, Private, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)