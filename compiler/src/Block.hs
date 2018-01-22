module Block where

import Data.List
import Text.Show.Functions
import Data.Char
import Data.Maybe

-- Boolean expressions
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr

-- Boolean ops
data BBinOp
  = And
  | Or

-- R binary ops
data RBinOp
  = Greater
  | Less

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr

-- Arithmetic ops
data ABinOp
  = OpeningParenthesis
  | ClosingParenthesis
  | Add
  | Subtract
  | Multiply
  | Divide

-- Statements
data Expr
  = Seq [Expr]
  | Module String [Expr]
  | Import String String
  | MainFunction {name ::String, argTypes:: [Expr], args::[Expr], returnType::Expr, body::[Expr]}
  | Function String [Expr] [Expr] Expr [Expr]
  | FunctionCall String [Expr]
  | Type String
  | ValueType String
  | Argument String
  | ArgumentType String
  | ReturnType String
  | AssignArith Expr String AExpr
  | AssignString Expr String Expr
  | If BExpr [Expr] [Expr]
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
  | Skip

instance Show ABinOp where
    show (Add) = "+"
    show (Subtract) = "-"
    show (Multiply) = "*"
    show (Divide) = "/"

instance Show BBinOp where
    show (And) = "&&"
    show (Or) = "||"

instance Show RBinOp where
    show (Greater) = ">"
    show (Less) = "<"

instance Show AExpr where
    show (Var v) = v
    show (IntConst i) = show i
    show (Neg aExpr) = "-" ++ show aExpr
    show (ABinary aBinOp aExpr1 aExpr2) = show aExpr1 ++ " " ++ show aBinOp ++ " " ++ show aExpr2

instance Show BExpr where
    show (BoolConst b) = lowerString $ show b
    show (Not n) = show n
    show (BBinary bbinop bExpr1 bExpr2) = show bExpr1 ++ " " ++ show bbinop ++ " " ++ show bExpr2
    show (RBinary rbinop aExpr1 aExpr2) = show aExpr1 ++ " " ++ show rbinop ++ " " ++ show aExpr2

instance Show Expr where
    show (Module name bodyArray) =
        -- Get the main function tree

        "public class " ++ name ++ "{\n" ++
            "public static void main(String[] args){\n" ++
                name ++ " " ++ lowerString name ++ "= new " ++ name ++ "();\n" ++
                intercalate "\n" (map (\mStatement -> if(isFunctionCall mStatement) then (lowerString name ++ "." ++ show mStatement) else show mStatement) (body ((filter (isMainFunction) bodyArray)!!0))) ++
            "}\n" ++
            getFunctionString bodyArray ++
        "}\n"

    show (Import directory moduleName) = "import " ++ directory ++ moduleName
    show (Function name argTypes args returnType body) = "public " ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
    show (MainFunction name argTypes args returnType body) =
        intercalate "\n " $ map show body
    show (FunctionCall name exprs) = name ++ "(" ++ (intercalate ", " (map show exprs)) ++ ");"
    show (Type b) = b
    show (Argument b) = b
    show (ArgumentType b) = b
    show (ReturnType b) = b
    show (AssignArith vType name value) = "" ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (AssignString vType name value) = "" ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (If condition statement elseStatement) = "if(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "} else {\n" ++ intercalate "\n" (map show elseStatement) ++ "}"
    show (While condition statement) = "while(" ++ show condition ++ "){\n" ++ intercalate "\n" (map show statement) ++ "}"
    show (Skip) = "[skip]"
    show (Seq s) = "[seq]"
    show (Return expr) = "return " ++ show expr ++ ";"
    show (Print exprs) = "System.out.println(" ++ exprs ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    show (ArrayDef arrType name) = arrType ++ "[] " ++ name ++ "="
    show (ArrayValues exprs) = "{" ++ intercalate ", " exprs ++ "};"
    show (ArrayAssignment arr values) = show arr ++ show values
    show (ArrayElementSelect i) = "[" ++ i ++ "];"
    show (Lambda valName collectionName) = ""
    show (Where exprs) = intercalate "\n" (map show exprs)
    show (StringLiteral value) = "\"" ++ value ++ "\""
    show (_) = "<unknown>"

lowerString str = [ toLower loweredString | loweredString <- str]

extractMain :: Expr -> Maybe String
extractMain (MainFunction m _ _ _ _) = Just m
extractMain _ = Nothing

extractFunctionCall :: Expr -> Maybe String
extractFunctionCall (FunctionCall m _) = Just m
extractFunctionCall _ = Nothing

isMainFunction :: Expr -> Bool
isMainFunction e = isJust $ extractMain e

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
getFunctionString :: [Expr] -> String
getFunctionString e = do
    if(isMainFunction (e!!0)) then
      ""
    else
      "" ++ show (e!!0) ++ getFunctionString (drop 1 e)
