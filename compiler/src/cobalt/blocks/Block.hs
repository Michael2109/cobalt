{-|
Module      : Block
Description : Data types that store general expressions.
These are used to store the data and specify how the code is generated.
-}

module Block where

import Data.List
import Text.Show.Functions
import Data.Char
import Data.Maybe

import ABBlock
import SymbolTable

debug :: Bool
debug = False

getDebug :: String -> String
getDebug message = (if debug then "<" ++ message ++ "> " else "")


-- Statements
data Expr
  = Seq [Expr]
  | Import {locs ::[String]}
  | GlobalVar String Bool Expr Expr [Expr]
  | MainFunction {name ::String, annotations :: (Maybe Expr), argTypes:: [Expr], args::[Expr], returnType::Expr, body::[Expr]}
  | Function String (Maybe Expr) [Expr] [Expr] Expr Bool [Expr]
  | Constructor String [Expr] [Expr] [Expr]
  | FunctionCall String [Expr]
  | Type Expr
  | ValueType String
  | Argument String
  | ArgumentType String
  | ReturnType String
  | AssignArith Bool Expr String Expr
  | ArithExpr AExpr
  | ArrayType String
  | ArrayAppend [Expr]
  | Assign Expr Expr Expr
  | Reassign String Expr
  | If BExpr [Expr]
  | ElseIf BExpr [Expr]
  | Else [Expr]
  | Try [Expr]
  | Catch (Maybe [Expr]) [Expr]
  | While Expr [Expr]
  | Print Expr
  | Return Expr
  | ArrayValues [String]
  | ArrayDef String String
  | ArrayAssignment Expr Expr
  | ArrayElementSelect String
  | Where [Expr]
  | StringLiteral String
  | Data String [Expr]
  | DataElement String String [String] [String]
  | DataInstance Expr [Expr]
  | SuperMethodCall String String [Expr]
  | ObjectMethodCall String String [Expr]
  | ThisMethodCall String [Expr]
  | NewClassInstance String [Expr]
  | ClassVariable String String
  | BooleanExpr BExpr
  | Identifier String
  | Annotation String
  | ModifierBlock [Expr]
  | This
  | Super
  | Lambda String [Expr]
  | ClassParam {varType :: Expr, varName :: Expr}
  | Skip

  -- Module specific
  | Module [String] String [Expr] [Expr]

  -- Class specific
  | Class [String]String (Maybe [Expr]) (Maybe String) (Maybe String) [Expr] [Expr] [Expr] [Expr]

instance ErrorCheck Expr where
  errorCheck (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = "An error occurred"
  errorCheck (_) = "<Unimplemented error check>"


instance CodeGen Expr where
    genCode (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable =
        getDebug "Class" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> genCode x symbolTable) imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> genCode x symbolTable) modifierBlocks) ++

        intercalate " " (map (\x -> "private final " ++ genCode x symbolTable ++ ";") paramList) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> genCode x symbolTable) paramList) ++"){" ++

        intercalate " " (map (\x -> "this." ++ genCode (varName x) symbolTable ++ "=" ++ genCode (varName x) symbolTable ++ ";") paramList) ++

        intercalate " " (map (\x -> genCode x symbolTable) constructorExprs) ++ "}" ++

        --intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map (\x -> genCode x symbolTable) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          paramList = case params of
              Just a -> a
              Nothing -> []
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              Just a -> "implements " ++ a
              Nothing -> ""
    genCode (Module packageLocs name imports bodyArray) symbolTable =
        getDebug "Module" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> genCode x symbolTable) imports) ++
        "public final class " ++ name ++ "{\n" ++
        intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map (\x -> genCode x symbolTable) (filter (not . isImportStatement) bodyArray))  ++
        "}"
    genCode (Import locs) symbolTable = getDebug "Import" ++ "import " ++ intercalate "." locs ++ ";"
    genCode (GlobalVar modifier final varType varName exprs) symbolTable =
      getDebug "GlobalVar" ++
      "private " ++ genCode varType symbolTable ++ " " ++ genCode varName symbolTable ++ "=" ++ intercalate " " (map (\x -> genCode x symbolTable) exprs) ++ ";" ++
      -- Bool to store if the value has been set yet
      "private boolean " ++ genCode varName symbolTable ++ "Bool=false;" ++
      -- Create getter method
      modifier ++ " " ++ genCode varType symbolTable ++ " " ++ genCode varName symbolTable ++ "(){ if(!" ++ genCode varName symbolTable ++ "Bool){" ++ genCode varName symbolTable ++ "Bool=true;" ++ genCode varName symbolTable ++ "=" ++ intercalate " " (map (\e -> genCode e symbolTable ++ ";") exprs)  ++ "}return " ++ genCode varName symbolTable ++ ";}" ++
      -- If it isn't final create a setter method
      if(not final)
        then modifier ++ " void " ++ genCode varName symbolTable ++ "_(final " ++ genCode varType symbolTable ++ " " ++ genCode varName symbolTable ++ "){this." ++ genCode varName symbolTable ++ "Bool=true;" ++ "this." ++ genCode varName symbolTable ++ "=" ++ genCode varName symbolTable ++ ";}"
        else ""

    genCode (Constructor name argTypes args body) symbolTable = getDebug "Constructor" ++ "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable) argTypes) (map (\x -> genCode x symbolTable) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) body) ++ "}"
    genCode (Function name annotations argTypes args returnType static body) symbolTable =
      getDebug "Function" ++
      annotationString ++ " public " ++ (if(static) then "static " else "") ++ genCode returnType symbolTable ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable) argTypes) (map (\x -> genCode x symbolTable) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> genCode a symbolTable
              Nothing -> ""
    genCode (MainFunction name annotations argTypes args returnType body) symbolTable =
      getDebug "MainFunction" ++
      annotationString ++ "public static " ++ genCode returnType symbolTable ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable) argTypes) (map (\x -> genCode x symbolTable) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> genCode a symbolTable
              Nothing -> ""
    genCode (FunctionCall name exprs) symbolTable = getDebug "FunctionCall" ++ name ++ "(" ++ (intercalate ", " (map (\x -> genCode x symbolTable) exprs)) ++ ");"
    genCode (Type b) symbolTable = getDebug "Type" ++ genCode b symbolTable
    genCode (Argument b) symbolTable = getDebug "Argument" ++ b
    genCode (ArgumentType b) symbolTable = getDebug "ArgumentType" ++ b
    genCode (ReturnType b) symbolTable = getDebug "ReturnType" ++ b
    genCode (AssignArith mutable vType name value) symbolTable = getDebug "AssignArith" ++ (if mutable then "" else "final ") ++ genCode vType symbolTable ++ " " ++ name ++ "=" ++ genCode value symbolTable ++ ";"
    genCode (ArithExpr aExpr) symbolTable = getDebug "ArithExpr" ++ genCode aExpr symbolTable
    genCode (Assign vType name value) symbolTable = getDebug "Assign" ++ genCode vType symbolTable ++ " " ++ genCode name symbolTable ++ "=" ++ genCode value symbolTable ++ ";"
    genCode (Reassign name value) symbolTable = getDebug "Reassign" ++ name ++ "_(" ++ genCode value symbolTable ++ ");"
    genCode (If condition statement) symbolTable = getDebug "If" ++ "if(" ++ genCode condition symbolTable ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) statement) ++ "}"
    genCode (ElseIf condition statement) symbolTable = getDebug "ElseIf" ++ " else if(" ++ genCode condition symbolTable ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) statement) ++ "}"
    genCode (Else statement) symbolTable = getDebug "Else" ++ " else {\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) statement) ++ "}"
    genCode (Try exprs) symbolTable = getDebug "Try" ++ "try{" ++ intercalate " " (map (\x -> genCode x symbolTable) exprs) ++ "}"
    genCode (Catch params exprs) symbolTable = getDebug "Catch" ++ "catch(" ++  intercalate ", " (map (\x -> "final " ++ genCode x symbolTable) paramList)  ++ "){" ++ intercalate " " (map (\x -> genCode x symbolTable) exprs) ++ "}"
      where
        paramList = case params of
            Just a -> a
            Nothing -> []
    genCode (While condition statement) symbolTable = getDebug "While" ++ "while(" ++ genCode condition symbolTable ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable) statement) ++ "}"
    genCode (Skip) symbolTable = getDebug "Skip" ++ "[skip]"
    genCode (Seq s) symbolTable = getDebug "Seq" ++ "[seq]"
    genCode (Return expr) symbolTable = getDebug "Return" ++ "return " ++ genCode expr symbolTable ++ ";"
    genCode (Print exprs) symbolTable = getDebug "Print" ++ "System.out.println(" ++ genCode exprs symbolTable ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    genCode (ArrayType arrType) symbolTable = getDebug "ArrayType" ++ arrType ++ "[]"
    genCode (ArrayDef arrType name) symbolTable = getDebug "ArrayDef" ++ arrType ++ "[] " ++ name ++ "="
    genCode (ArrayValues exprs) symbolTable = getDebug "ArrayValues" ++ "{" ++ intercalate ", " exprs ++ "};"
    genCode (ArrayAssignment arr values) symbolTable = getDebug "ArrayAssignment" ++ genCode arr symbolTable ++ genCode values symbolTable
    genCode (ArrayAppend arrays) symbolTable = getDebug "ArrayAppend" ++ intercalate "" (map (\arr -> "") arrays)
    genCode (ArrayElementSelect i) symbolTable = getDebug "ArrayElementSelect" ++ "[" ++ i ++ "];"
    genCode (Where exprs) symbolTable = getDebug "Where" ++ intercalate "\n" (map (\x -> genCode x symbolTable) exprs)
    genCode (StringLiteral value) symbolTable = getDebug "StringLiteral" ++ "\"" ++ value ++ "\""
    genCode (Data name exprs) symbolTable = getDebug "Data" ++ "class " ++ name ++ "{}" ++ intercalate " " (map (\x -> genCode x symbolTable) exprs)
    genCode (DataElement superName name argTypes args) symbolTable = getDebug "DataElement" ++ "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"
    genCode (DataInstance typeName args) symbolTable = getDebug "DataInstance" ++ "new " ++ genCode typeName symbolTable ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable) args) ++ ");"
    genCode (ThisMethodCall methodName args) symbolTable = getDebug "ThisMethodCall" ++methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable) args) ++ ");"
    genCode (SuperMethodCall objectName methodName args) symbolTable = getDebug "SuperMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable) args) ++ ");"
    genCode (ObjectMethodCall objectName methodName args) symbolTable = getDebug "ObjectMethodCall" ++objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable) args) ++ ");"
    genCode (NewClassInstance className args) symbolTable = getDebug "NewClassInstance" ++ "new " ++ className ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable) args) ++ ")"
    genCode (ClassVariable className varName) symbolTable = getDebug "ClassVariable" ++className ++ "." ++ varName
    genCode (BooleanExpr expr) symbolTable = getDebug "BooleanExpr" ++ genCode expr symbolTable
    genCode (Identifier name) symbolTable = getDebug "Identifier" ++ name
    genCode (Annotation name) symbolTable = getDebug "Annotation" ++ "@" ++ name
    genCode (ModifierBlock exprs) symbolTable = getDebug "ModifierBlock" ++ intercalate " " (map (\x -> genCode x symbolTable) exprs)
    genCode (This) symbolTable = getDebug "This" ++ "this"
    genCode (Super) symbolTable = getDebug "Super" ++ "super"
    genCode (Lambda varName exprs) symbolTable = getDebug "Lambda" ++ "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map (\x -> genCode x symbolTable) exprs)
    genCode (ClassParam varType varName) symbolTable = getDebug "ClassParam" ++ genCode varType symbolTable ++ " " ++ genCode varName symbolTable
    genCode (_) symbolTable = "<unknown>"

lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: Expr -> Maybe [String]
extractImportStatement (Import m) = Just m
extractImportStatement _ = Nothing

extractMainFunction :: Expr -> Maybe String
extractMainFunction (MainFunction m _ _ _ _ _) = Just m
extractMainFunction _ = Nothing

extractFunctionCall :: Expr -> Maybe String
extractFunctionCall (FunctionCall m _) = Just m
extractFunctionCall _ = Nothing


isImportStatement :: Expr -> Bool
isImportStatement e = isJust $ extractImportStatement e

isMainFunction :: Expr -> Bool
isMainFunction e = isJust $ extractMainFunction e

isFunctionCall :: Expr -> Bool
isFunctionCall e = isJust $ extractFunctionCall e

{--
getInnerMainFunctionString :: [Expr] -> String -> String
getInnerMainFunctionString e instanceName  = do
    if(isMainFunction (e!!0)) then
      show (e!!0)
    else
      getInnerMainFunctionString (drop 1 e) instanceName
--}
getFunctionString :: Expr -> ClassSymbolTable -> String
getFunctionString e symbolTable = do
    if(isMainFunction (e) || isImportStatement (e)) then
      ""
    else
      "" ++ genCode (e) symbolTable
