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
  genSymbolTable,
  AExpr (Var, IntConst, Neg, ABinary, Parenthesis),
  BExpr (BoolConst, Not, BBinary, RBinary),
  ABinOp (Multiply, Divide, Add, Subtract),
  BBinOp (Or, And),
  RBinOp (Greater, Less, GreaterEqual, LessEqual)
) where

import SymbolTable
import BlockUtils


class ErrorCheck a where
  errorCheck :: a -> String

class SymbolTableGen a where
  genSymbolTable :: a -> ClassSymbolTable

class CodeGen a where
  genCode :: a -> ClassSymbolTable -> CurrentState -> String

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr
  deriving (Eq)

instance CodeGen AExpr where
    genCode (Var v) symbolTable currentState = do
      let methodList = map (snd) (filter (\x -> fst x == (method currentState)) (methods symbolTable))
      if(length methodList > 0)
      then if (elem v (map fst (methodArgs (methodList!!0)))) then v else (v ++ "()")
      else v
    genCode (IntConst i) symbolTable currentState = show i
    genCode (Neg aExpr) symbolTable currentState = "-" ++ genCode aExpr symbolTable currentState
    genCode (ABinary aBinOp aExpr1 aExpr2) symbolTable currentState = genCode aExpr1 symbolTable currentState ++ " " ++ genCode aBinOp symbolTable currentState ++ " " ++ genCode aExpr2 symbolTable currentState
    genCode (Parenthesis aExpr) symbolTable currentState = "(" ++ genCode aExpr symbolTable currentState ++ ")"

-- Arithmetic ops
data ABinOp
  = OpeningParenthesis
  | ClosingParenthesis
  | Add
  | Subtract
  | Multiply
  | Divide
  deriving (Eq)

instance CodeGen ABinOp where
    genCode (Add) symbolTable currentState = "+"
    genCode (Subtract) symbolTable currentState = "-"
    genCode (Multiply) symbolTable currentState = "*"
    genCode (Divide) symbolTable currentState = "/"
    genCode (OpeningParenthesis) currentState symbolTable = "("
    genCode (ClosingParenthesis) currentState symbolTable = ")"


-- Boolean expressions
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Eq)

instance CodeGen BExpr where
    genCode (BoolConst b) symbolTable currentState = lowerString $ show b
    genCode (Not n) symbolTable currentState = genCode n symbolTable currentState
    genCode (BBinary bbinop bExpr1 bExpr2) symbolTable currentState = genCode bExpr1 symbolTable currentState ++ " " ++ genCode bbinop symbolTable currentState ++ " " ++ genCode bExpr2 symbolTable currentState
    genCode (RBinary rbinop aExpr1 aExpr2) symbolTable currentState = genCode aExpr1 symbolTable currentState ++ " " ++ genCode rbinop symbolTable currentState ++ " " ++ genCode aExpr2 symbolTable currentState


-- Boolean ops
data BBinOp
  = And
  | Or
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
  deriving (Eq)

instance CodeGen RBinOp where
    genCode (Greater) symbolTable currentState = ">"
    genCode (Less) symbolTable currentState = "<"
    genCode (GreaterEqual) symbolTable currentState = ">="
    genCode (LessEqual) symbolTable currentState = "<="