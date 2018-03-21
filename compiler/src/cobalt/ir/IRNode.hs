module IRNode where

import Data.List
import Data.Maybe
import Data.Char
import Text.Format

import BlockUtils
import SymbolTable

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
  pretty :: a -> SymbolTable -> CurrentState -> String -- Pretty document type

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
  | ArrayAssignmentIR IRInfo IRNode IRNode
  | ArrayDefIR IRInfo String String
  | ArrayElementSelectIR IRInfo String
  | ArrayTypeIR IRInfo String
  | ArrayValuesIR IRInfo [String]
  | AssignIR IRInfo IRNode IRNode IRNode
  | AssignArithIR IRInfo Bool IRNode String IRNode
  | BBinaryIR IRInfo IRNode IRNode IRNode
  | BBinOpErrorIR IRInfo
  | BErrorIR IRInfo
  | BoolConstIR IRInfo Bool
  | BooleanExprIR IRInfo IRNode
  | CatchIR IRInfo [IRNode] [IRNode]
  | ClassIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ClassParamIR IRInfo IRNode IRNode
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
  | FunctionIR IRInfo String (Maybe IRNode) [IRNode] [IRNode] IRNode Bool [IRNode]
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
  | MainFunctionIR IRInfo String (Maybe IRNode) [IRNode] [IRNode] IRNode [IRNode]
  | ModifierBlockIR IRInfo [IRNode]
  | MultiplyIR IRInfo
  | NegIR IRInfo IRNode
  | NewClassInstanceIR IRInfo String [IRNode]
  | NotIR IRInfo IRNode
  | ObjectIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ObjectMethodCallIR IRInfo String String [IRNode]
  | OpeningParenthesisIR IRInfo
  | OrIR IRInfo
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
  | SuperMethodCallIR IRInfo String String [IRNode]
  | ThisIR IRInfo
  | ThisMethodCallIR IRInfo String [IRNode]
  | ThisVarIR IRInfo IRNode
  | TraitIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | TryIR IRInfo [IRNode]
  | TypeIR IRInfo IRNode
  | VarIR IRInfo String
  | WhereIR IRInfo [IRNode]
  | WhileIR IRInfo IRNode [IRNode]
  deriving (Eq, Show)


instance Pretty IRNode where
    pretty (ABinaryIR irInfo aBinOp aExpr1 aExpr2) symbolTable currentState = ""
    pretty (AddIR irInfo) symbolTable currentState = ""
    pretty (AndIR irInfo) symbolTable currentState = ""
    pretty (AnnotationIR irInfo name) symbolTable currentState = show irInfo
    pretty (ArgumentIR irInfo b) symbolTable currentState = show irInfo
    pretty (ArgumentTypeIR irInfo b) symbolTable currentState = show irInfo
    pretty (ArithExprIR irInfo aExpr) symbolTable currentState = show irInfo
    pretty (ArrayAppendIR irInfo arrays) symbolTable currentState = show irInfo
    pretty (ArrayAssignmentIR irInfo arr values) symbolTable currentState = show irInfo
    pretty (ArrayDefIR irInfo arrType name) symbolTable currentState = show irInfo
    pretty (ArrayElementSelectIR irInfo i) symbolTable currentState = show irInfo
    pretty (ArrayTypeIR irInfo arrType) symbolTable currentState = show irInfo
    pretty (ArrayValuesIR irInfo exprs) symbolTable currentState = show irInfo
    pretty (AssignIR irInfo vType name value) symbolTable currentState = show irInfo
    pretty (AssignArithIR irInfo mutable vType name value) symbolTable currentState = show irInfo
    pretty (BBinaryIR irInfo  bbinop bExpr1 bExpr2) st cs = ""
    pretty (BoolConstIR irInfo  b) st cs = ""
    pretty (BooleanExprIR irInfo expr) symbolTable currentState = show irInfo
    pretty (CatchIR irInfo params exprs) symbolTable currentState = show irInfo
    pretty (ClassIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = show irInfo
    pretty (ClassParamIR irInfo varType varName) symbolTable currentState = show irInfo ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState
    pretty (ClassVariableIR irInfo className varName) symbolTable currentState = show irInfo
    pretty (ClosingParenthesisIR irInfo) currentState symbolTable = ""
    pretty (ConstructorIR irInfo name argTypes args body) symbolTable currentState = show irInfo
    pretty (DataIR irInfo name exprs) symbolTable currentState = show irInfo
    pretty (DataElementIR irInfo superName name argTypes args) symbolTable currentState = show irInfo
    pretty (DivideIR irInfo) symbolTable currentState = ""
    pretty (ElseIR irInfo statement) symbolTable currentState = show irInfo
    pretty (ElseIfIR irInfo condition statement) symbolTable currentState = show irInfo
    pretty (ForIR irInfo varName start end exprs) symbolTable currentState = show irInfo
    pretty (FunctionIR irInfo name annotations argTypes args returnType static body) symbolTable currentState = show irInfo
    pretty (FunctionCallIR irInfo name exprs) symbolTable currentState = show irInfo
    pretty (GlobalVarIR irInfo modifier final static varType varName exprs) symbolTable currentState = show irInfo
    pretty (GreaterEqualIR irInfo) symbolTable currentState = ""
    pretty (GreaterIR irInfo) symbolTable currentState = ""
    pretty (IdentifierIR irInfo name) symbolTable currentState = show irInfo
    pretty (IfIR irInfo condition statement) symbolTable currentState = show irInfo
    pretty (ImportIR irInfo locs) symbolTable currentState = show irInfo
    pretty (IntConstIR irInfo i) symbolTable currentState = ""
    pretty (LambdaIR irInfo varName exprs) symbolTable currentState = show irInfo
    pretty (LessEqualIR irInfo) symbolTable currentState = ""
    pretty (LessIR irInfo) symbolTable currentState = ""
    pretty (MainFunctionIR irInfo name annotations argTypes args returnType body) symbolTable currentState = show irInfo
    pretty (ModifierBlockIR irInfo exprs) symbolTable currentState = show irInfo
    pretty (MultiplyIR irInfo) symbolTable currentState = ""
    pretty (NegIR irInfo aExpr) symbolTable currentState = ""
    pretty (NewClassInstanceIR irInfo className args) symbolTable currentState = show irInfo
    pretty (NotIR irInfo  n) st cs = ""
    pretty (ObjectIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = show irInfo
    pretty (ObjectMethodCallIR irInfo objectName methodName args) symbolTable currentState = show irInfo
    pretty (OrIR irInfo) symbolTable currentState = ""
    pretty (OpeningParenthesisIR irInfo) currentState symbolTable = ""
    pretty (ParenthesisIR irInfo aExpr) symbolTable currentState = ""
    pretty (PrintIR irInfo exprs) symbolTable currentState = show irInfo
    pretty (RBinaryIR irInfo  rbinop aExpr1 aExpr2) st cs = ""
    pretty (ReassignIR irInfo name value) symbolTable currentState = show irInfo
    pretty (ReturnIR irInfo expr) symbolTable currentState = show irInfo
    pretty (ReturnTypeIR irInfo b) symbolTable currentState = show irInfo
    pretty (SeqIR irInfo s) symbolTable currentState = show irInfo
    pretty (SkipIR irInfo) symbolTable currentState = show irInfo
    pretty (StringLiteralIR irInfo value) symbolTable currentState = show irInfo
    pretty (SubtractIR irInfo) symbolTable currentState = ""
    pretty (SuperIR irInfo) symbolTable currentState = show irInfo
    pretty (SuperMethodCallIR irInfo objectName methodName args) symbolTable currentState = show irInfo
    pretty (ThisIR irInfo) symbolTable currentState = show irInfo ++ "this"
    pretty (ThisMethodCallIR irInfo methodName args) symbolTable currentState = show irInfo
    pretty (ThisVarIR irInfo varName) symbolTable currentState = show irInfo
    pretty (TraitIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = show irInfo
    pretty (TryIR irInfo exprs) symbolTable currentState = show irInfo
    pretty (TypeIR irInfo b) symbolTable currentState = show irInfo
    pretty (VarIR irInfo v) symbolTable currentState = ""
    pretty (WhereIR irInfo exprs) symbolTable currentState = show irInfo
    pretty (WhileIR irInfo condition statement) symbolTable currentState = show irInfo
    pretty (Empty irInfo) symbolTable currentState = show irInfo


lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: IRNode -> Maybe [String]
extractImportStatement (ImportIR _ m) = Just m
extractImportStatement _ = Nothing

isImportStatement :: IRNode -> Bool
isImportStatement e = isJust $ extractImportStatement e
