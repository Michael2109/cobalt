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

instance Show BExpr where
    show (BoolConst b) = lowerString $ show b
    show (Not n) = show n
    show (BBinary bbinop bExpr1 bExpr2) = show bExpr1 ++ " " ++ show bbinop ++ " " ++ show bExpr2
    show (RBinary rbinop aExpr1 aExpr2) = show aExpr1 ++ " " ++ show rbinop ++ " " ++ show aExpr2


-- Boolean ops
data BBinOp
  = And
  | Or

instance Show BBinOp where
    show (And) = "&&"
    show (Or) = "||"

-- R binary ops
data RBinOp
  = Greater
  | Less

instance Show RBinOp where
    show (Greater) = ">"
    show (Less) = "<"

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr

instance Show AExpr where
    show (Var v) = v
    show (IntConst i) = show i
    show (Neg aExpr) = "-" ++ show aExpr
    show (ABinary aBinOp aExpr1 aExpr2) = show aExpr1 ++ " " ++ show aBinOp ++ " " ++ show aExpr2
    show (Parenthesis aExpr) = "(" ++ show aExpr ++ ")"

-- Arithmetic ops
data ABinOp
  = OpeningParenthesis
  | ClosingParenthesis
  | Add
  | Subtract
  | Multiply
  | Divide

instance Show ABinOp where
    show (Add) = "+"
    show (Subtract) = "-"
    show (Multiply) = "*"
    show (Divide) = "/"
    show (OpeningParenthesis) = "("
    show (ClosingParenthesis) = ")"

-- Statements
data Expr
  = Seq [Expr]
  | Module String [Expr]
  | Import [String]
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
  | DataElement String String [String]
  | Skip

instance Show Expr where
    show (Module name bodyArray) =
        -- Get the main function tree
        intercalate "\n" (map (show) (filter isImportStatement bodyArray)) ++
        "public class " ++ name ++ "{\n" ++

            "public static void main(String[] args){\n" ++
                name ++ " " ++ lowerString name ++ "= new " ++ name ++ "();\n" ++
                intercalate "\n" (map (\mStatement -> if(isFunctionCall mStatement) then (lowerString name ++ "." ++ show mStatement) else show mStatement) (body ((filter (isMainFunction) bodyArray)!!0))) ++
            "}\n" ++

            intercalate "\n" (map (getFunctionString) bodyArray) ++
        "}\n"

    show (Import locs) = "import " ++ intercalate "." locs ++ ";"
    show (Function name argTypes args returnType body) = "public " ++ show returnType ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map show argTypes) (map show args)) ++"){\n" ++ intercalate "\n" (map show body) ++ "}"
    show (MainFunction name argTypes args returnType body) = intercalate "\n " $ map show body
    show (FunctionCall name exprs) = name ++ "(" ++ (intercalate ", " (map show exprs)) ++ ");"
    show (Type b) = b
    show (Argument b) = b
    show (ArgumentType b) = b
    show (ReturnType b) = b
    show (AssignArith vType name value) = "" ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
    show (AssignString vType name value) = "" ++ show vType ++ " " ++ name ++ "=" ++ show value ++ ";"
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
    show (ArrayElementSelect i) = "[" ++ i ++ "];"
    show (Lambda valName collectionName) = ""
    show (Where exprs) = intercalate "\n" (map show exprs)
    show (StringLiteral value) = "\"" ++ value ++ "\""
    show (Data name exprs) = "class " ++ name ++ "{}" ++ intercalate " " (map show exprs)
    show (DataElement superName name argTypes) = "class " ++ name ++ " extends "++ superName ++" { public " ++ name ++ "(" ++ intercalate ", " (map showDataElement (zip argTypes [0..])) ++ "){" ++ "} }"
    show (_) = "<unknown>"

showDataElement (name, index) = name ++ " " ++ (lowerString name) ++ (show index)

lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: Expr -> Maybe [String]
extractImportStatement (Import m) = Just m
extractImportStatement _ = Nothing

extractMainFunction :: Expr -> Maybe String
extractMainFunction (MainFunction m _ _ _ _) = Just m
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
