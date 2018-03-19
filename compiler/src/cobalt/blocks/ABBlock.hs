{-|
Module      : ABBlock
Description : Data types that store arithmetic and boolean expressions.
These are used to store the data and specify how the code is generated.
-}
module ABBlock (
  CodeGen,
  ErrorCheck,
  SymbolTableGen,
  genCode,
  errorCheck,
  genClassSymbolTable,
  AExpr (Var, IntConst, Neg, ABinary, Parenthesis, AError),
  BExpr (BoolConst, Not, BBinary, RBinary, BError),
  ABinOp (Multiply, Divide, Add, Subtract, ABinOpError),
  BBinOp (Or, And, BBinOpError),
  RBinOp (Greater, Less, GreaterEqual, LessEqual, RBinOpError)
) where

import SymbolTable
import IRNode

class Debug a where
  debug :: a -> String

class ErrorCheck a where
  errorCheck :: a -> String

class SymbolTableGen a where
  genClassSymbolTable :: a -> ClassSymbolTable

class CodeGen a where
  genCode :: a -> SymbolTable -> CurrentState -> IRNode

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr
  | AError
  deriving (Eq)

instance CodeGen AExpr where
    genCode (Var v) symbolTable currentState = do
      -- todo
      --let methodList = map (snd) (filter (\x -> fst x == (currentMethodName currentState)) (methods symbolTable))
      --if(length methodList > 0)
      --then if (elem v (map fst (methodArgs (methodList!!0)))) then v else (v ++ "()")
      --else v
      VarIR (IRInfo "var") v
    genCode (IntConst i) symbolTable currentState = IntConstIR (IRInfo "IntConst") i
    genCode (Neg aExpr) symbolTable currentState = "-" ++ genCode aExpr symbolTable currentState
    genCode (ABinary aBinOp aExpr1 aExpr2) symbolTable currentState = genCode aExpr1 symbolTable currentState ++ " " ++ genCode aBinOp symbolTable currentState ++ " " ++ genCode aExpr2 symbolTable currentState
    genCode (Parenthesis aExpr) symbolTable currentState = "(" ++ genCode aExpr symbolTable currentState ++ ")"

instance Show AExpr where
  show (Var v) = v
  show (IntConst i) = show i
  show (Neg aExpr) = show aExpr
  show (ABinary aBinOp aExpr1 aExpr2) = show aBinOp ++ show aExpr1 ++ show aExpr2
  show (Parenthesis aExpr) = show aExpr
  show (AError) = "<AError>"

-- Arithmetic ops
data ABinOp
  = OpeningParenthesis
  | ClosingParenthesis
  | Add
  | Subtract
  | Multiply
  | Divide
  | ABinOpError
  deriving (Eq)

instance CodeGen ABinOp where
    genCode (Add) symbolTable currentState = "+"
    genCode (Subtract) symbolTable currentState = "-"
    genCode (Multiply) symbolTable currentState = "*"
    genCode (Divide) symbolTable currentState = "/"
    genCode (OpeningParenthesis) currentState symbolTable = "("
    genCode (ClosingParenthesis) currentState symbolTable = ")"

instance Show ABinOp where
  show (Add) = "+"
  show (Subtract) = "-"
  show (Multiply) = "*"
  show (Divide) = "/"
  show (OpeningParenthesis) = "("
  show (ClosingParenthesis) = ")"
  show (ABinOpError) = "<ABinOpError>"

-- Boolean expressions
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  | BError
  deriving (Eq)

instance CodeGen BExpr where
    genCode (BoolConst b) symbolTable currentState = lowerString $ show b
    genCode (Not n) symbolTable currentState = genCode n symbolTable currentState
    genCode (BBinary bbinop bExpr1 bExpr2) symbolTable currentState = genCode bExpr1 symbolTable currentState ++ " " ++ genCode bbinop symbolTable currentState ++ " " ++ genCode bExpr2 symbolTable currentState
    genCode (RBinary rbinop aExpr1 aExpr2) symbolTable currentState = genCode aExpr1 symbolTable currentState ++ " " ++ genCode rbinop symbolTable currentState ++ " " ++ genCode aExpr2 symbolTable currentState

instance Show BExpr where
  show (BoolConst v) = if v then "true" else "false"
  show (RBinary rbinop aExpr1 aExpr2) = show rbinop ++ show aExpr1 ++ show aExpr2
  show (BError) = "<BError>"
  show (_) = "<UNKNOWN SHOW BEXPR>"

-- Boolean ops
data BBinOp
  = And
  | Or
  | BBinOpError
  deriving (Eq)

instance CodeGen BBinOp where
    genCode (And) symbolTable currentState = "&&"
    genCode (Or) symbolTable currentState = "||"

-- R binary ops
data RBinOp
  = Greater
  | GreaterEqual
  | Less
  | LessEqual
  | RBinOpError
  deriving (Eq)

instance CodeGen RBinOp where
    genCode (Greater) symbolTable currentState = ">"
    genCode (Less) symbolTable currentState = "<"
    genCode (GreaterEqual) symbolTable currentState = ">="
    genCode (LessEqual) symbolTable currentState = "<="

instance Show RBinOp where
  show (Greater) = ">"
  show (Less) = "<"
  show (GreaterEqual) = ">="
  show (LessEqual) = "<="
  show (RBinOpError) = "<RBinOpError>"