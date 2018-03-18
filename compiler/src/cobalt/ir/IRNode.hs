module IRNode where

import ABBlock
import Block

data IRTree
  = SeqIR IRInfo [Expr]
  | ImportIR IRInfo [String]
  | GlobalVarIR IRInfo  String Bool Bool Expr Expr [Expr]
  | MainFunctionIR IRInfo String (Maybe Expr) [Expr] [Expr] Expr [Expr]
  | FunctionIR IRInfo  String (Maybe Expr) [Expr] [Expr] Expr Bool [Expr]
  | ConstructorIR IRInfo  String [Expr] [Expr] [Expr]
  | FunctionCallIR IRInfo  String [Expr]
  | TypeIR IRInfo  Expr
  | ArgumentIR IRInfo  Expr
  | ArgumentTypeIR IRInfo  String
  | ReturnTypeIR IRInfo  String
  | AssignArithIR IRInfo  Bool Expr String Expr
  | ArithExprIR IRInfo  AExpr
  | ArrayTypeIR IRInfo  String
  | ArrayAppendIR IRInfo  [Expr]
  | AssignIR IRInfo  Expr Expr Expr
  | ReassignIR IRInfo  Expr Expr
  | IfIR IRInfo  Expr [Expr]
  | ElseIfIR IRInfo  Expr [Expr]
  | ElseIR IRInfo  [Expr]
  | TryIR IRInfo  [Expr]
  | CatchIR IRInfo  (Maybe [Expr]) [Expr]
  | WhileIR IRInfo  Expr [Expr]
  | ForIR IRInfo  String Expr Expr [Expr]
  | PrintIR IRInfo  Expr
  | ReturnIR IRInfo  Expr
  | ArrayValuesIR IRInfo  [String]
  | ArrayDefIR IRInfo  String String
  | ArrayAssignmentIR IRInfo  Expr Expr
  | ArrayElementSelectIR IRInfo  String
  | WhereIR IRInfo  [Expr]
  | StringLiteralIR IRInfo  String
  | DataIR IRInfo  String [Expr]
  | DataElementIR IRInfo  String String [String] [String]
  | DataInstanceIR IRInfo  Expr [Expr]
  | SuperMethodCallIR IRInfo  String String [Expr]
  | ObjectMethodCallIR IRInfo  String String [Expr]
  | ThisVarIR IRInfo  Expr
  | ThisMethodCallIR IRInfo  String [Expr]
  | NewClassInstanceIR IRInfo  String [Expr]
  | ClassVariableIR IRInfo  String String
  | BooleanExprIR IRInfo  BExpr
  | IdentifierIR IRInfo  String
  | AnnotationIR IRInfo  String
  | ModifierBlockIR IRInfo  [Expr]
  | ThisIR IRInfo
  | SuperIR IRInfo
  | LambdaIR IRInfo  String [Expr]
  | ClassParamIR IRInfo Expr Expr
  | SkipIR IRInfo
  | ErrorIR IRInfo
  | ObjectIR IRInfo [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ClassIR IRInfo  [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | TraitIR IRInfo  [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  deriving (Eq)

data IRInfo = IRInfo String
  deriving (Eq)