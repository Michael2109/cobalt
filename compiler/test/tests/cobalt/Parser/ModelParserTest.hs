module Parser.ModelParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
--
--import AST.Data.Modifier
import Parser.ExprParser

testModelParser :: Test
testModelParser = do
    let code = unlines [ "class Test" ]
    TestCase $ assertEqual code
        (Model (Name "Test") [] [] Nothing [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> error "Didn't parse correctly"
             Right x -> x)

testModelParserInner :: Test
testModelParserInner = do
    let code = unlines [ "class OuterClass"
                       , "    class InnerClass"]
    TestCase $ assertEqual code
        (Model {modelName = Name "OuterClass", modelModifiers = [], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [], modelMethods = [ModelDef (Model {modelName = Name "InnerClass", modelModifiers = [], modelFields = [], modelParent = Nothing, modelParentArguments = [], modelInterfaces = [], modelMethods = []})]})
        (case (parse (modelParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)

{--

testModelParserClass :: Test
testModelParserClass = do
    let code = unlines [ "class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserTrait :: Test
testModelParserTrait = do
    let code = unlines [ "trait Test" ]
    TestCase $ assertEqual code
        (Trait Nothing "Test" Nothing [] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserObject :: Test
testModelParserObject = do
    let code = unlines [ "object Test" ]
    TestCase $ assertEqual code
        (Object Nothing "Test" Nothing [] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserParamsEmpty :: Test
testModelParserParamsEmpty = do
    let code = unlines [ "class Test ()" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserParamsSingle :: Test
testModelParserParamsSingle = do
    let code = unlines [ "class Test (x: Int)" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [Parameter (Identifier "Int") (Identifier "x")] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserParamsMultiple :: Test
testModelParserParamsMultiple = do
    let code = unlines [ "class Test (x: Int, y: String, z: Object)" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [Parameter (Identifier "Int") (Identifier "x"),Parameter (Identifier "String") (Identifier "y"),Parameter (Identifier "Object") (Identifier "z")] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserTypeParameter :: Test
testModelParserTypeParameter = do
    let code = unlines [ "class Test[String]" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserTypeParameterExtends :: Test
testModelParserTypeParameterExtends = do
    let code = unlines [ "class Test[String] extends ParentName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserTypeParameterExtendsImplements :: Test
testModelParserTypeParameterExtendsImplements = do
    let code = unlines [ "class Test[String] extends ParentName implements TraitName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [] ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserTypeParameterImplements :: Test
testModelParserTypeParameterImplements = do
    let code = unlines [ "class Test[String] implements TraitName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing [] ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserTypeParameterImplementsMultiple :: Test
testModelParserTypeParameterImplementsMultiple = do
    let code = unlines [ "class Test[String] implements TraitName1, TraitName2" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing [] ["TraitName1","TraitName2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserExtends :: Test
testModelParserExtends = do
    let code = unlines [ "class Test extends ParentClass" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserParentArgsEmpty :: Test
testModelParserParentArgsEmpty = do
    let code = unlines [ "class Test[String] extends ParentName ()" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserParentArgsSingle :: Test
testModelParserParentArgsSingle = do
    let code = unlines [ "class Test[String] extends ParentName (x)" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [Identifier "x"] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserParentArgsMultiple :: Test
testModelParserParentArgsMultiple = do
    let code = unlines [ "class Test[String] extends ParentName (x, y, z, 100, \"test\")" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [Identifier "x",Identifier "y",Identifier "z",IntConst 100,StringLiteral "test"] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserImplements :: Test
testModelParserImplements = do
    let code = unlines [ "class Test implements Interface" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing [] ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserImplementsMultiple :: Test
testModelParserImplementsMultiple = do
    let code = unlines [ "class Test implements Interface1, Interface2" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing [] ["Interface1","Interface2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserExtendsImplements :: Test
testModelParserExtendsImplements = do
    let code = unlines [ "class Test extends ParentClass implements Interface" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserExtendsImplementsMultiple :: Test
testModelParserExtendsImplementsMultiple = do
    let code = unlines [ "class Test extends ParentClass implements Interface1, Interface2, Interface3" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] ["Interface1","Interface2","Interface3"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserImports :: Test
testModelParserImports = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] ["Interface"] imports [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserImportsFail :: Test
testModelParserImportsFail = do
    let code = unlines [ "import -dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       ]
    TestCase $ assertEqual code
        Error
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserModifierBlock :: Test
testModelParserModifierBlock = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "  let x: int = 5"
                       ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] ["Interface"] [Import ["dir","sub_dir","ClassName"]] [ModifierBlock [Assign True (Just (Type (Identifier "int"))) (Identifier "x") (IntConst 5)]] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserConstructorBody :: Test
testModelParserConstructorBody = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "  let x: int = 5"
                       , "initAlien(10, 20)"
                       ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] ["Interface"] [Import ["dir","sub_dir","ClassName"]] [ModifierBlock [Assign True (Just (Type (Identifier "int"))) (Identifier "x") (IntConst 5)]] [MethodCall "initAlien" [IntConst 10,IntConst 20]] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserMethods :: Test
testModelParserMethods = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "    let x: int = 5"
                       , "initAlien(10, 20)"
                       , "exampleMethod (a: Int, b: Int): Int"
                       , "initAlien (x: int, y: int): void"
                       , "    this.x <- x"
                       , "    this.y <- y"
                       , "act (direction: int): void"
                       ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] ["Interface"] [Import ["dir","sub_dir","ClassName"]] [ModifierBlock [Assign True (Just (Type (Identifier "int"))) (Identifier "x") (IntConst 5)]] [MethodCall "initAlien" [IntConst 10,IntConst 20]] [Method (Identifier "exampleMethod") Nothing [] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [],Method (Identifier "initAlien") Nothing [] [Parameter (Identifier "int") (Identifier "x"),Parameter (Identifier "int") (Identifier "y")] (Identifier "void") False [Reassign (ThisVar (Identifier "x")) (Identifier "x"),Reassign (ThisVar (Identifier "y")) (Identifier "y")],Method (Identifier "act") Nothing [] [Parameter (Identifier "int") (Identifier "direction")] (Identifier "void") False []])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Modifiers
testModelParserPublic :: Test
testModelParserPublic = do
    let code = unlines [ "public class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Public] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserProtected :: Test
testModelParserProtected = do
    let code = unlines [ "protected class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Protected] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserPrivate :: Test
testModelParserPrivate = do
    let code = unlines [ "private class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Private] [] Nothing []  [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserPublicAbstract :: Test
testModelParserPublicAbstract = do
    let code = unlines [ "public abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Public, Abstract] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserProtectedAbstract :: Test
testModelParserProtectedAbstract = do
    let code = unlines [ "protected abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Protected, Abstract] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserPrivateAbstract :: Test
testModelParserPrivateAbstract = do
    let code = unlines [ "private abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Private, Abstract] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserAbstract :: Test
testModelParserAbstract = do
    let code = unlines [ "abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Abstract] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserPublicFinal :: Test
testModelParserPublicFinal = do
    let code = unlines [ "public final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Public, Final] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserProtectedFinal :: Test
testModelParserProtectedFinal = do
    let code = unlines [ "protected final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Protected, Final] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserPrivateFinal :: Test
testModelParserPrivateFinal = do
    let code = unlines [ "private final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Private, Final] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserFinal :: Test
testModelParserFinal = do
    let code = unlines [ "final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Final] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserReordered1 :: Test
testModelParserReordered1 = do
    let code = unlines [ "abstract public class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Abstract, Public] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserReordered2 :: Test
testModelParserReordered2 = do
    let code = unlines [ "final abstract public class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Final, Abstract, Public] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testModelParserReordered3 :: Test
testModelParserReordered3 = do
    let code = unlines [ "abstract private final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Abstract, Private, Final] [] Nothing [] [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)
             --}
