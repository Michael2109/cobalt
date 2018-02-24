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

debug :: Bool
debug = False

getDebug :: String -> String
getDebug message = (if debug then "<" ++ message ++ "> " else "")

-- Statements
data Expr
  = Seq [Expr]
  | Import {locs ::[String]}
  | GlobalVar String Expr Expr [Expr]
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

instance Show Expr where
    show (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) =
        getDebug "Class" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map show imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map show modifierBlocks) ++

        intercalate " " (map (\x -> "private final " ++ show x ++ ";") paramList) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map show paramList) ++"){" ++

        intercalate " " (map (\x -> "this." ++ show (varName x) ++ "=" ++ show (varName x) ++ ";") paramList) ++

        intercalate " " (map show constructorExprs) ++ "}" ++

        --intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map show (filter (not . isImportStatement) bodyArray))  ++
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
    show (Module packageLocs name imports bodyArray) =
        getDebug "Module" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map show imports) ++
        "public final class " ++ name ++ "{\n" ++
        intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map (show) (filter (not . isImportStatement) bodyArray))  ++
        "}"
    show (Import locs) = getDebug "Import" ++ "import " ++ intercalate "." locs ++ ";"
    show (GlobalVar modifier varType varName exprs) =
      getDebug "GlobalVar" ++
      modifier ++ " " ++ show varType ++ " " ++ show varName ++ "=" ++ intercalate " " (map show exprs) ++ ";" ++
      modifier ++ " " ++ show varType ++ " " ++ show varName ++ "(){" ++ intercalate " " (map (\e -> "return " ++ show e ++ ";") exprs) ++ "}"
    show (Constructor name argTypes args body) = getDebug "Constructor" ++ "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
    show (Function name annotations argTypes args returnType static body) =
      getDebug "Function" ++
      annotationString ++ "public " ++ (if(static) then "static " else "") ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> show a
              Nothing -> ""
    show (MainFunction name annotations argTypes args returnType body) =
      getDebug "MainFunction" ++
      annotationString ++ "public static " ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> show a
              Nothing -> ""
    show (FunctionCall name exprs) = getDebug "FunctionCall" ++ name ++ "(" ++ (intercalate ", " (map show exprs)) ++ ");"
    show (Type b) = getDebug "Type" ++ show b
    show (Argument b) = getDebug "Argument" ++ b
    show (ArgumentType b) = getDebug "ArgumentType" ++ b
    show (ReturnType b) = getDebug "ReturnType" ++ b
    show (AssignArith mutable vType name value) = getDebug "AssignArith" ++ (if mutable then "" else "final ") ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (ArithExpr aExpr) = getDebug "ArithExpr" ++ show aExpr
    show (Assign vType name value) = getDebug "Assign" ++ show vType ++ " " ++ show name ++ "=" ++ show value ++ ";"
    show (Reassign name value) = getDebug "Reassign" ++ name ++ "=" ++ show value ++ ";"
    show (If condition statement) = getDebug "If" ++ "if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (ElseIf condition statement) = getDebug "ElseIf" ++ " else if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Else statement) = getDebug "Else" ++ " else {\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Try exprs) = getDebug "Try" ++ "try{" ++ intercalate " " (map show exprs) ++ "}"
    show (Catch params exprs) = getDebug "Catch" ++ "catch(" ++  intercalate ", " (map (\x -> "final " ++ show x) paramList)  ++ "){" ++ intercalate " " (map show exprs) ++ "}"
      where
        paramList = case params of
            Just a -> a
            Nothing -> []
    show (While condition statement) = getDebug "While" ++ "while(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Skip) = getDebug "Skip" ++ "[skip]"
    show (Seq s) = getDebug "Seq" ++ "[seq]"
    show (Return expr) = getDebug "Return" ++ "return " ++ show expr ++ ";"
    show (Print exprs) = getDebug "Print" ++ "System.out.println(" ++ show exprs ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    show (ArrayType arrType) = getDebug "ArrayType" ++ arrType ++ "[]"
    show (ArrayDef arrType name) = getDebug "ArrayDef" ++ arrType ++ "[] " ++ name ++ "="
    show (ArrayValues exprs) = getDebug "ArrayValues" ++ "{" ++ intercalate ", " exprs ++ "};"
    show (ArrayAssignment arr values) = getDebug "ArrayAssignment" ++ show arr ++ show values
    show (ArrayAppend arrays) = getDebug "ArrayAppend" ++ intercalate "" (map (\arr -> "") arrays)
    show (ArrayElementSelect i) = getDebug "ArrayElementSelect" ++ "[" ++ i ++ "];"
    show (Where exprs) = getDebug "Where" ++ intercalate "\n" (map show exprs)
    show (StringLiteral value) = getDebug "StringLiteral" ++ "\"" ++ value ++ "\""
    show (Data name exprs) = getDebug "Data" ++ "class " ++ name ++ "{}" ++ intercalate " " (map show exprs)
    show (DataElement superName name argTypes args) = getDebug "DataElement" ++ "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"
    show (DataInstance typeName args) = getDebug "DataInstance" ++ "new " ++ show typeName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (ThisMethodCall methodName args) = getDebug "ThisMethodCall" ++methodName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (SuperMethodCall objectName methodName args) = getDebug "SuperMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (ObjectMethodCall objectName methodName args) = getDebug "ObjectMethodCall" ++objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (NewClassInstance className args) = getDebug "NewClassInstance" ++ "new " ++ className ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (ClassVariable className varName) = getDebug "ClassVariable" ++className ++ "." ++ varName
    show (BooleanExpr expr) = getDebug "BooleanExpr" ++ show expr
    show (Identifier name) = getDebug "Identifier" ++ name
    show (Annotation name) = getDebug "Annotation" ++ "@" ++ name
    show (ModifierBlock exprs) = getDebug "ModifierBlock" ++ intercalate " " (map show exprs)
    show (This) = getDebug "This" ++ "this"
    show (Super) = getDebug "Super" ++ "super"
    show (Lambda varName exprs) = getDebug "Lambda" ++ "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map show exprs)
    show (ClassParam varType varName) = getDebug "ClassParam" ++ show varType ++ " " ++ show varName
    show (_) = "<unknown>"

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
getFunctionString :: Expr -> String
getFunctionString e = do
    if(isMainFunction (e) || isImportStatement (e)) then
      ""
    else
      "" ++ show (e)
