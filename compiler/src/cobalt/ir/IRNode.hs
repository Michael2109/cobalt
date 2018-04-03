{-|
Module      : IRNode
Description : Contains functions for working with intermediate representation tree.
-}
module IRNode where

import Data.Char
import Data.List
import Data.Maybe
import Text.Format
import Text.PrettyPrint.Annotated.Leijen

import SymbolTable
import Utils

debug :: Bool
debug = False

getDebug :: String -> String
getDebug message = (if debug then "<" ++ message ++ "> " else "")

{--import Text.PrettyPrint.ANSI.Leijen
       ( Doc
       , int, text, char
       , (</>), (<//>), (<+>), (<>)
       , parens
       )--}

class Pretty a where
  pretty :: a -> SymbolTable -> CurrentState -> (Doc String) -- Pretty document type

data IRInfo = IRInfo String
  deriving (Eq)

instance Show IRInfo where
  show (IRInfo info) = if debug then format "/*{0}*/" [info] else ""

data IRNode
  = ABinaryIR IRInfo IRNode IRNode IRNode
  | ABinOpErrorIR IRInfo
  | AddIR IRInfo
  | AndIR IRInfo
  | AErrorIR IRInfo
  | AnnotationIR IRInfo String
  | ArgumentIR IRInfo IRNode
  | ArgumentTypeIR IRInfo String
  | ArithExprIR IRInfo IRNode
  | ArrayAppendIR IRInfo [IRNode]
  | ArrayElementSelectIR IRInfo String
  | ArrayValuesIR IRInfo [String]
  | AssignIR IRInfo Bool (Maybe IRNode) IRNode IRNode
  | AssignArithIR IRInfo Bool IRNode String IRNode
  | BBinaryIR IRInfo IRNode IRNode IRNode
  | BBinOpErrorIR IRInfo
  | BErrorIR IRInfo
  | BoolConstIR IRInfo Bool
  | BooleanExprIR IRInfo IRNode
  | CatchIR IRInfo [IRNode] [IRNode]
  | ClassIR IRInfo (Maybe IRNode) String (Maybe IRNode) [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ParameterIR IRInfo IRNode IRNode
  | ClassVariableIR IRInfo String String
  | ClosingParenthesisIR IRInfo
  | ConstructorIR IRInfo String [IRNode] [IRNode] [IRNode]
  | DataIR IRInfo String [IRNode]
  | DataElementIR IRInfo String String [String] [String]
  | DataInstanceIR IRInfo IRNode [IRNode]
  | DivideIR IRInfo
  | ElseIR IRInfo [IRNode]
  | ElseIfIR IRInfo IRNode [IRNode]
  | Empty IRInfo
  | ErrorIR IRInfo
  | ForIR IRInfo String IRNode IRNode [IRNode]
  | FunctionIR IRInfo IRNode (Maybe IRNode) [IRNode] IRNode Bool [IRNode]
  | FunctionCallIR IRInfo String [IRNode]
  | GlobalVarIR IRInfo String Bool Bool IRNode IRNode [IRNode]
  | GreaterEqualIR IRInfo
  | GreaterIR IRInfo
  | IdentifierIR IRInfo String
  | IfIR IRInfo IRNode [IRNode]
  | ImportIR IRInfo [String]
  | LambdaIR IRInfo String [IRNode]
  | LessIR IRInfo
  | LessEqualIR IRInfo
  | IntConstIR IRInfo Integer
  | MainFunctionIR IRInfo IRNode (Maybe IRNode) [IRNode] IRNode [IRNode]
  | MethodCallIR IRInfo String [IRNode]
  | ModifierBlockIR IRInfo [IRNode]
  | MultiplyIR IRInfo
  | NegIR IRInfo IRNode
  | NewClassInstanceIR IRInfo IRNode [IRNode]
  | NotIR IRInfo IRNode
  | ObjectIR IRInfo (Maybe IRNode) String (Maybe IRNode) [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ObjectMethodCallIR IRInfo String String [IRNode]
  | OpeningParenthesisIR IRInfo
  | OrIR IRInfo
  | PackageIR IRInfo [String]
  | ParameterizedTypeIR IRInfo IRNode IRNode
  | ParenthesisIR IRInfo IRNode
  | PrintIR IRInfo IRNode
  | ReassignIR IRInfo IRNode IRNode
  | ReturnIR IRInfo IRNode
  | ReturnTypeIR IRInfo String
  | RBinaryIR IRInfo IRNode IRNode IRNode
  | RBinOpErrorIR IRInfo
  | SeqIR IRInfo [IRNode]
  | SkipIR IRInfo
  | StringLiteralIR IRInfo String
  | SubtractIR IRInfo
  | SuperIR IRInfo
  | SuperMethodCallIR IRInfo String [IRNode]
  | ThisIR IRInfo
  | ThisMethodCallIR IRInfo String [IRNode]
  | ThisVarIR IRInfo IRNode
  | TraitIR IRInfo (Maybe IRNode) String (Maybe IRNode) [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | TryIR IRInfo [IRNode]
  | TypeIR IRInfo IRNode
  | TypeParameterIR IRInfo IRNode
  | WhereIR IRInfo [IRNode]
  | WhileIR IRInfo IRNode [IRNode]
  deriving (Eq, Show)

instance Pretty IRNode where
  pretty (ABinaryIR irInfo aBinOp aExpr1 aExpr2) symbolTable currentState = p $ show irInfo
  pretty (AddIR irInfo) symbolTable currentState = p $ show irInfo
  pretty (AndIR irInfo) symbolTable currentState = p $ show irInfo
  pretty (AnnotationIR irInfo name) symbolTable currentState = p $ show irInfo
  pretty (ArgumentIR irInfo b) symbolTable currentState = p $ show irInfo
  pretty (ArgumentTypeIR irInfo b) symbolTable currentState = p $ show irInfo
  pretty (ArithExprIR irInfo aExpr) symbolTable currentState = p $ show irInfo
  pretty (ArrayAppendIR irInfo arrays) symbolTable currentState = p $ show irInfo
  pretty (ArrayElementSelectIR irInfo i) symbolTable currentState = p $ show irInfo
  pretty (ArrayValuesIR irInfo exprs) symbolTable currentState = p $ show irInfo
  pretty (AssignIR irInfo immutable vType name value) symbolTable currentState = p $ show irInfo
  pretty (AssignArithIR irInfo mutable vType name value) symbolTable currentState = p $ show irInfo
  pretty (BBinaryIR irInfo  bbinop bExpr1 bExpr2) st cs = p $ ""
  pretty (BoolConstIR irInfo  b) st cs = p $ ""
  pretty (BooleanExprIR irInfo expr) symbolTable currentState = p $ show irInfo
  pretty (CatchIR irInfo params exprs) symbolTable currentState = p $ show irInfo
  pretty (ClassIR irInfo package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = p $ show irInfo
  pretty (ClassVariableIR irInfo className varName) symbolTable currentState = p $ show irInfo
  pretty (ClosingParenthesisIR irInfo) currentState symbolTable = p $ ""
  pretty (ConstructorIR irInfo name argTypes args body) symbolTable currentState = p $ show irInfo
  pretty (DataIR irInfo name exprs) symbolTable currentState = p $ show irInfo
  pretty (DataElementIR irInfo superName name argTypes args) symbolTable currentState = p $ show irInfo
  pretty (DivideIR irInfo) symbolTable currentState = p $ ""
  pretty (ElseIR irInfo statement) symbolTable currentState = p $ show irInfo
  pretty (ElseIfIR irInfo condition statement) symbolTable currentState = p $ show irInfo
  pretty (ForIR irInfo varName start end exprs) symbolTable currentState = p $ show irInfo
  pretty (FunctionIR irInfo name annotations params returnType static body) symbolTable currentState = p $ show irInfo
  pretty (FunctionCallIR irInfo name exprs) symbolTable currentState = p $ show irInfo
  pretty (GlobalVarIR irInfo modifier final static varType varName exprs) symbolTable currentState = p $ show irInfo
  pretty (GreaterEqualIR irInfo) symbolTable currentState = p $ ""
  pretty (GreaterIR irInfo) symbolTable currentState = p $ ""
  pretty (IdentifierIR irInfo name) symbolTable currentState = p $ show irInfo
  pretty (IfIR irInfo condition statement) symbolTable currentState = p $ show irInfo
  pretty (ImportIR irInfo locs) symbolTable currentState = p $ show irInfo
  pretty (IntConstIR irInfo i) symbolTable currentState = p $ ""
  pretty (LambdaIR irInfo varName exprs) symbolTable currentState = p $ show irInfo
  pretty (LessEqualIR irInfo) symbolTable currentState = p $ ""
  pretty (LessIR irInfo) symbolTable currentState = p $ ""
  pretty (MainFunctionIR irInfo name annotations params returnType body) symbolTable currentState = p $ show irInfo
  pretty (MethodCallIR irInfo methodName args) symbolTable currentState = p $ show irInfo
  pretty (ModifierBlockIR irInfo exprs) symbolTable currentState = p $ show irInfo
  pretty (MultiplyIR irInfo) symbolTable currentState = p $ ""
  pretty (NegIR irInfo aExpr) symbolTable currentState = p $ ""
  pretty (NewClassInstanceIR irInfo className args) symbolTable currentState = p $ show irInfo
  pretty (NotIR irInfo  n) st cs = p $ ""
  pretty (ObjectIR irInfo package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = p $ show irInfo
  pretty (ObjectMethodCallIR irInfo objectName methodName args) symbolTable currentState = p $ show irInfo
  pretty (OrIR irInfo) symbolTable currentState = p $ ""
  pretty (OpeningParenthesisIR irInfo) currentState symbolTable = p $ ""
  pretty (PackageIR irInfo locs) symbolTable currentState = p $ show irInfo
  pretty (ParameterizedTypeIR irInfo className typeName) currentState symbolTable = p $ show irInfo
  pretty (ParameterIR irInfo varType varName) symbolTable currentState = p $ show irInfo
  pretty (ParenthesisIR irInfo aExpr) symbolTable currentState = p $ ""
  pretty (PrintIR irInfo exprs) symbolTable currentState = p $ show irInfo
  pretty (RBinaryIR irInfo  rbinop aExpr1 aExpr2) st cs = p $ ""
  pretty (ReassignIR irInfo name value) symbolTable currentState = p $ show irInfo
  pretty (ReturnIR irInfo expr) symbolTable currentState = p $ show irInfo
  pretty (ReturnTypeIR irInfo b) symbolTable currentState = p $ show irInfo
  pretty (SeqIR irInfo s) symbolTable currentState = p $ show irInfo
  pretty (SkipIR irInfo) symbolTable currentState = p $ show irInfo
  pretty (StringLiteralIR irInfo value) symbolTable currentState = p $ show irInfo
  pretty (SubtractIR irInfo) symbolTable currentState = p $ ""
  pretty (SuperIR irInfo) symbolTable currentState = p $ show irInfo
  pretty (SuperMethodCallIR irInfo methodName args) symbolTable currentState = p $ show irInfo
  pretty (ThisIR irInfo) symbolTable currentState = p $ show irInfo ++ "this"
  pretty (ThisMethodCallIR irInfo methodName args) symbolTable currentState = p $ show irInfo
  pretty (ThisVarIR irInfo varName) symbolTable currentState = p $ show irInfo
  pretty (TraitIR irInfo package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = p $ show irInfo
  pretty (TryIR irInfo exprs) symbolTable currentState = p $ show irInfo
  pretty (TypeIR irInfo b) symbolTable currentState = p $ show irInfo
  pretty (TypeParameterIR irInfo typeName) symbolTable currentState = p $ show irInfo
  pretty (WhereIR irInfo exprs) symbolTable currentState = p $ show irInfo
  pretty (WhileIR irInfo condition statement) symbolTable currentState = p $ show irInfo
  pretty (Empty irInfo) symbolTable currentState = p $ show irInfo

-- | Pretty-print inside a precedence context to avoid parentheses.
-- Consider + to be 6, * to be 7.
p :: String -> Doc String
p s = text s

extractImportStatement :: IRNode -> Maybe [String]
extractImportStatement (ImportIR _ m) = Just m
extractImportStatement _ = Nothing

isImportStatement :: IRNode -> Bool
isImportStatement e = isJust $ extractImportStatement e
