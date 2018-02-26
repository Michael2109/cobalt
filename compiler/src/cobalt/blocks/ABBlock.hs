{-|
Module      : ABBlock
Description : Data types that store arithmetic and boolean expressions.
These are used to store the data and specify how the code is generated.
-}
module ABBlock where

import BlockUtils

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr

instance Show AExpr where
    show (Var v) = v ++ "()"
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