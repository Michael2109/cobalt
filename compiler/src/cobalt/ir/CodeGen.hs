{-|
Module      : IRNode
Description : Contains functions for working with intermediate representation tree.
-}
module CodeGen where

import Data.Char
import Data.List
import Data.Maybe
import Text.Format
import Text.PrettyPrint.Annotated.Leijen
import Control.Monad
import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B

import SymbolTable
import Utils
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import qualified Java.Lang
import qualified Java.IO

--class CodeGen a where
--  genCode :: (Throws UnexpectedEndMethod e) => GenerateIO e ()

{--
data CodeGenNode
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

instance CodeGen IRNode where
  genCode (ABinaryIR irInfo aBinOp aExpr1 aExpr2) symbolTable currentState = p $ show irInfo
  genCode (AddIR irInfo) symbolTable currentState = p $ show irInfo
  genCode (AndIR irInfo) symbolTable currentState = p $ show irInfo
  genCode (AnnotationIR irInfo name) symbolTable currentState = p $ show irInfo
  genCode (ArgumentIR irInfo b) symbolTable currentState = p $ show irInfo
  genCode (ArgumentTypeIR irInfo b) symbolTable currentState = p $ show irInfo
  genCode (ArithExprIR irInfo aExpr) symbolTable currentState = p $ show irInfo
  genCode (ArrayAppendIR irInfo arrays) symbolTable currentState = p $ show irInfo
  genCode (ArrayElementSelectIR irInfo i) symbolTable currentState = p $ show irInfo
  genCode (ArrayValuesIR irInfo exprs) symbolTable currentState = p $ show irInfo
  genCode (AssignIR irInfo immutable vType name value) symbolTable currentState = p $ show irInfo
  genCode (AssignArithIR irInfo mutable vType name value) symbolTable currentState = p $ show irInfo
  genCode (BBinaryIR irInfo  bbinop bExpr1 bExpr2) st cs = p $ ""
  genCode (BoolConstIR irInfo  b) st cs = p $ ""
  genCode (BooleanExprIR irInfo expr) symbolTable currentState = p $ show irInfo
  genCode (CatchIR irInfo params exprs) symbolTable currentState = p $ show irInfo
  genCode (ClassIR irInfo package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = p $ show irInfo
  genCode (ClassVariableIR irInfo className varName) symbolTable currentState = p $ show irInfo
  genCode (ClosingParenthesisIR irInfo) currentState symbolTable = p $ ""
  genCode (ConstructorIR irInfo name argTypes args body) symbolTable currentState = p $ show irInfo
  genCode (DataIR irInfo name exprs) symbolTable currentState = p $ show irInfo
  genCode (DataElementIR irInfo superName name argTypes args) symbolTable currentState = p $ show irInfo
  genCode (DivideIR irInfo) symbolTable currentState = p $ ""
  genCode (ElseIR irInfo statement) symbolTable currentState = p $ show irInfo
  genCode (ElseIfIR irInfo condition statement) symbolTable currentState = p $ show irInfo
  genCode (ForIR irInfo varName start end exprs) symbolTable currentState = p $ show irInfo
  genCode (FunctionIR irInfo name annotations params returnType static body) symbolTable currentState = p $ show irInfo
  genCode (FunctionCallIR irInfo name exprs) symbolTable currentState = p $ show irInfo
  genCode (GlobalVarIR irInfo modifier final static varType varName exprs) symbolTable currentState = p $ show irInfo
  genCode (GreaterEqualIR irInfo) symbolTable currentState = p $ ""
  genCode (GreaterIR irInfo) symbolTable currentState = p $ ""
  genCode (IdentifierIR irInfo name) symbolTable currentState = p $ show irInfo
  genCode (IfIR irInfo condition statement) symbolTable currentState = p $ show irInfo
  genCode (ImportIR irInfo locs) symbolTable currentState = p $ show irInfo
  genCode (IntConstIR irInfo i) symbolTable currentState = p $ ""
  genCode (LambdaIR irInfo varName exprs) symbolTable currentState = p $ show irInfo
  genCode (LessEqualIR irInfo) symbolTable currentState = p $ ""
  genCode (LessIR irInfo) symbolTable currentState = p $ ""
  genCode (MainFunctionIR irInfo name annotations params returnType body) symbolTable currentState = p $ show irInfo
  genCode (MethodCallIR irInfo methodName args) symbolTable currentState = p $ show irInfo
  genCode (ModifierBlockIR irInfo exprs) symbolTable currentState = p $ show irInfo
  genCode (MultiplyIR irInfo) symbolTable currentState = p $ ""
  genCode (NegIR irInfo aExpr) symbolTable currentState = p $ ""
  genCode (NewClassInstanceIR irInfo className args) symbolTable currentState = p $ show irInfo
  genCode (NotIR irInfo  n) st cs = p $ ""
  genCode (ObjectIR irInfo package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = p $ show irInfo
  genCode (ObjectMethodCallIR irInfo objectName methodName args) symbolTable currentState = p $ show irInfo
  genCode (OrIR irInfo) symbolTable currentState = p $ ""
  genCode (OpeningParenthesisIR irInfo) currentState symbolTable = p $ ""
  genCode (PackageIR irInfo locs) symbolTable currentState = p $ show irInfo
  genCode (ParameterizedTypeIR irInfo className typeName) currentState symbolTable = p $ show irInfo
  genCode (ParameterIR irInfo varType varName) symbolTable currentState = p $ show irInfo
  genCode (ParenthesisIR irInfo aExpr) symbolTable currentState = p $ ""
  genCode (PrintIR irInfo exprs) symbolTable currentState = p $ show irInfo
  genCode (RBinaryIR irInfo  rbinop aExpr1 aExpr2) st cs = p $ ""
  genCode (ReassignIR irInfo name value) symbolTable currentState = p $ show irInfo
  genCode (ReturnIR irInfo expr) symbolTable currentState = p $ show irInfo
  genCode (ReturnTypeIR irInfo b) symbolTable currentState = p $ show irInfo
  genCode (SeqIR irInfo s) symbolTable currentState = p $ show irInfo
  genCode (SkipIR irInfo) symbolTable currentState = p $ show irInfo
  genCode (StringLiteralIR irInfo value) symbolTable currentState = p $ show irInfo
  genCode (SubtractIR irInfo) symbolTable currentState = p $ ""
  genCode (SuperIR irInfo) symbolTable currentState = p $ show irInfo
  genCode (SuperMethodCallIR irInfo methodName args) symbolTable currentState = p $ show irInfo
  genCode (ThisIR irInfo) symbolTable currentState = p $ show irInfo ++ "this"
  genCode (ThisMethodCallIR irInfo methodName args) symbolTable currentState = p $ show irInfo
  genCode (ThisVarIR irInfo varName) symbolTable currentState = p $ show irInfo
  genCode (TraitIR irInfo package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = p $ show irInfo
  genCode (TryIR irInfo exprs) symbolTable currentState = p $ show irInfo
  genCode (TypeIR irInfo b) symbolTable currentState = p $ show irInfo
  genCode (TypeParameterIR irInfo typeName) symbolTable currentState = p $ show irInfo
  genCode (WhereIR irInfo exprs) symbolTable currentState = p $ show irInfo
  genCode (WhileIR irInfo condition statement) symbolTable currentState = p $ show irInfo
  genCode (Empty irInfo) symbolTable currentState = p $ show irInfo
--}