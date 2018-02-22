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
  | Catch String String [Expr]
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
  | Skip

  -- Module specific
  | Module [String] String [Expr] [Expr]

  -- Class specific
  | Class [String] String (Maybe String) (Maybe String) [Expr] [Expr] [Expr]

instance Show Expr where
    show (Class packageLocs name parent interfaces imports modifierBlocks bodyArray) = do
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map show imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map show modifierBlocks) ++
        --intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map show (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              Just a -> "implements " ++ a
              Nothing -> ""
    show (Module packageLocs name imports bodyArray) =
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map show imports) ++
        "public final class " ++ name ++ "{\n" ++
        intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map (show) (filter (not . isImportStatement) bodyArray))  ++
        "}"
    show (Import locs) = "import " ++ intercalate "." locs ++ ";"
    show (GlobalVar modifier varType varName exprs) =
      modifier ++ " " ++ show varType ++ " " ++ show varName ++ "=" ++ intercalate " " (map show exprs) ++ ";" ++
      modifier ++ " " ++ show varType ++ " " ++ show varName ++ "(){" ++ intercalate " " (map (\e -> "return " ++ show e ++ ";") exprs) ++ "}"
    show (Constructor name argTypes args body) = "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
    show (Function name annotations argTypes args returnType static body) =do
      annotationString ++ "public " ++ (if(static) then "static " else "") ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> show a
              Nothing -> ""
    show (MainFunction name annotations argTypes args returnType body) = do
      annotationString ++ "public static " ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> show a
              Nothing -> ""
    show (FunctionCall name exprs) = name ++ "(" ++ (intercalate ", " (map show exprs)) ++ ");"
    show (Type b) = show b
    show (Argument b) = b
    show (ArgumentType b) = b
    show (ReturnType b) = b
    show (AssignArith mutable vType name value) = "" ++ (if mutable then "" else "final ") ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (ArithExpr aExpr) = show aExpr
    show (Assign vType name value) = show vType ++ " " ++ show name ++ "=" ++ show value ++ ";"
    show (Reassign name value) = name ++ "=" ++ show value ++ ";"
    show (If condition statement) = "if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (ElseIf condition statement) = " else if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Else statement) = " else {\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Try exprs) = "try{" ++ intercalate " " (map show exprs) ++ "}"
    show (Catch argType argName exprs) = "catch(" ++ argType ++ " " ++ argName ++ "){" ++ intercalate " " (map show exprs) ++ "}"
    show (While condition statement) = "while(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Skip) = "[skip]"
    show (Seq s) = "[seq]"
    show (Return expr) = "return " ++ show expr ++ ";"
    show (Print exprs) = "System.out.println(" ++ show exprs ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    show (ArrayType arrType) = arrType ++ "[]"
    show (ArrayDef arrType name) = arrType ++ "[] " ++ name ++ "="
    show (ArrayValues exprs) = "{" ++ intercalate ", " exprs ++ "};"
    show (ArrayAssignment arr values) = show arr ++ show values
    show (ArrayAppend arrays) = intercalate "" (map (\arr -> "") arrays)
    show (ArrayElementSelect i) = "[" ++ i ++ "];"
    show (Where exprs) = intercalate "\n" (map show exprs)
    show (StringLiteral value) = "\"" ++ value ++ "\""
    show (Data name exprs) = "class " ++ name ++ "{}" ++ intercalate " " (map show exprs)
    show (DataElement superName name argTypes args) = "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"
    show (DataInstance typeName args) = "new " ++ show typeName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (ThisMethodCall methodName args) = methodName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (SuperMethodCall objectName methodName args) = objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (ObjectMethodCall objectName methodName args) = objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map show args) ++ ");"
    show (NewClassInstance className args) = "new " ++ className ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (ClassVariable className varName) = className ++ "." ++ varName
    show (BooleanExpr expr) = show expr
    show (Identifier name) = name
    show (Annotation name) = "@" ++ name
    show (ModifierBlock exprs) = intercalate " " (map show exprs)
    show (This) = "this"
    show (Super) = "super"
    show (Lambda varName exprs) = "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map show exprs)
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
