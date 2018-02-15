module Block where

import Data.List
import Text.Show.Functions
import Data.Char
import Data.Maybe

import ABBlock

-- Statements
data Expr
  = Seq [Expr]
  | Module [String] String [Expr] [Expr]
  | Import {locs ::[String]}
  | MainFunction {moduleName:: String, name ::String, argTypes:: [Expr], args::[Expr], returnType::Expr, body::[Expr]}
  | Function String String [Expr] [Expr] Expr [Expr]
  | FunctionCall String String [Expr]
  | Type String
  | ValueType String
  | Argument String
  | ArgumentType String
  | ReturnType String
  | AssignArith Expr String AExpr
  | ArrayAppend [Expr]
  | Assign Expr String Expr
  | If BExpr [Expr]
  | ElseIf BExpr [Expr]
  | Else [Expr]
  | While BExpr [Expr]
  | Print String
  | Return Expr
  | ArrayValues [String]
  | ArrayDef String String
  | ArrayAssignment Expr Expr
  | ArrayElementSelect String
  | Lambda String String
  | Where [Expr]
  | StringLiteral String
  | Data String [Expr]
  | DataElement String String [String] [String]
  | DataInstance String Expr Expr
  | Skip

instance Show Expr where
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
    show (Function moduleName name argTypes args returnType body) = "public " ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
    show (MainFunction moduleName name argTypes args returnType body) = "public static void main(String args[]){" ++  moduleName ++ " " ++ lowerString moduleName ++ "= new " ++ moduleName ++ "();\n" ++ (intercalate " " $ map show body) ++ "}"
    show (FunctionCall moduleName name exprs) = (if(length moduleName > 0) then lowerString moduleName ++ "." else "") ++ name ++ "(" ++ (intercalate ", " (map show exprs)) ++ ");"
    show (Type b) = b
    show (Argument b) = b
    show (ArgumentType b) = b
    show (ReturnType b) = b
    show (AssignArith vType name value) = "" ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (Assign vType name value) = "" ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (If condition statement) = "if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (ElseIf condition statement) = " else if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Else statement) = " else {\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (While condition statement) = "while(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Skip) = "[skip]"
    show (Seq s) = "[seq]"
    show (Return expr) = "return " ++ show expr ++ ";"
    show (Print exprs) = "System.out.println(" ++ exprs ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    show (ArrayDef arrType name) = arrType ++ "[] " ++ name ++ "="
    show (ArrayValues exprs) = "{" ++ intercalate ", " exprs ++ "};"
    show (ArrayAssignment arr values) = show arr ++ show values
    show (ArrayAppend arrays) = intercalate "" (map (\arr -> "") arrays)
    show (ArrayElementSelect i) = "[" ++ i ++ "];"
    show (Lambda valName collectionName) = ""
    show (Where exprs) = intercalate "\n" (map show exprs)
    show (StringLiteral value) = "\"" ++ value ++ "\""
    show (Data name exprs) = "class " ++ name ++ "{}" ++ intercalate " " (map show exprs)
    show (DataElement superName name argTypes args) = "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"
    show (DataInstance moduleName typeName expr) = "new " ++ moduleName ++ "().new " ++ show typeName ++ "(" ++ show expr ++ ");"
    show (_) = "<unknown>"

lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: Expr -> Maybe [String]
extractImportStatement (Import m) = Just m
extractImportStatement _ = Nothing

extractMainFunction :: Expr -> Maybe String
extractMainFunction (MainFunction m _ _ _ _ _) = Just m
extractMainFunction _ = Nothing

extractFunctionCall :: Expr -> Maybe String
extractFunctionCall (FunctionCall m _ _) = Just m
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
