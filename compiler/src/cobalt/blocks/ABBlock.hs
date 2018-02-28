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
  RBinOp (Greater, Less)
) where

import SymbolTable
import BlockUtils


class ErrorCheck a where
  errorCheck :: a -> String

class SymbolTableGen a where
  genSymbolTable :: a -> ClassSymbolTable

class CodeGen a where
  genCode :: a -> ClassSymbolTable -> String

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr

instance CodeGen AExpr where
    genCode (Var v) symbolTable = v ++ if(elem v (map (fst) $ publicVariables symbolTable )) then "()" else ""
    genCode (IntConst i) symbolTable = show i
    genCode (Neg aExpr) symbolTable = "-" ++ genCode aExpr symbolTable
    genCode (ABinary aBinOp aExpr1 aExpr2) symbolTable = genCode aExpr1 symbolTable ++ " " ++ genCode aBinOp symbolTable ++ " " ++ genCode aExpr2 symbolTable
    genCode (Parenthesis aExpr) symbolTable = "(" ++ genCode aExpr symbolTable ++ ")"

-- Arithmetic ops
data ABinOp
  = OpeningParenthesis
  | ClosingParenthesis
  | Add
  | Subtract
  | Multiply
  | Divide

instance CodeGen ABinOp where
    genCode (Add) symbolTable = "+"
    genCode (Subtract) symbolTable = "-"
    genCode (Multiply) symbolTable = "*"
    genCode (Divide) symbolTable = "/"
    genCode (OpeningParenthesis) symbolTable = "("
    genCode (ClosingParenthesis) symbolTable = ")"


-- Boolean expressions
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr

instance CodeGen BExpr where
    genCode (BoolConst b) symbolTable = lowerString $ show b
    genCode (Not n) symbolTable = genCode n symbolTable
    genCode (BBinary bbinop bExpr1 bExpr2) symbolTable = genCode bExpr1 symbolTable ++ " " ++ genCode bbinop symbolTable ++ " " ++ genCode bExpr2 symbolTable
    genCode (RBinary rbinop aExpr1 aExpr2) symbolTable = genCode aExpr1 symbolTable ++ " " ++ genCode rbinop symbolTable ++ " " ++ genCode aExpr2 symbolTable


-- Boolean ops
data BBinOp
  = And
  | Or

instance CodeGen BBinOp where
    genCode (And) symbolTable = "&&"
    genCode (Or) symbolTable = "||"

-- R binary ops
data RBinOp
  = Greater
  | Less

instance CodeGen RBinOp where
    genCode (Greater) symbolTable = ">"
    genCode (Less) symbolTable = "<"