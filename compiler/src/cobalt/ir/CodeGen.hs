{-|
Module      : CodeGen
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

class CodeGen a where
   genCode :: a -> Generate e ()

data CodeGenNode
    = ABinaryCodeGen CodeGenNode CodeGenNode CodeGenNode
    | ABinOpErrorCodeGen
    | AddCodeGen
    | AndCodeGen
    | AErrorCodeGen
    | AnnotationCodeGen  String
    | ArgumentCodeGen  CodeGenNode
    | ArgumentTypeCodeGen  String
    | ArithExprCodeGen  CodeGenNode
    | ArrayAppendCodeGen  [CodeGenNode]
    | ArrayElementSelectCodeGen  String
    | ArrayValuesCodeGen  [String]
    | AssignCodeGen  Bool (Maybe CodeGenNode) CodeGenNode CodeGenNode
    | AssignArithCodeGen  Bool CodeGenNode String CodeGenNode
    | BBinaryCodeGen  CodeGenNode CodeGenNode CodeGenNode
    | BBinOpErrorCodeGen
    | BErrorCodeGen
    | BoolConstCodeGen  Bool
    | BooleanExprCodeGen  CodeGenNode
    | CatchCodeGen  [CodeGenNode] [CodeGenNode]
    | ClassCodeGen  (Maybe CodeGenNode) String (Maybe CodeGenNode) [CodeGenNode] (Maybe String) [String] [CodeGenNode] [CodeGenNode] [CodeGenNode] [CodeGenNode]
    | ParameterCodeGen  CodeGenNode CodeGenNode
    | ClassVariableCodeGen  String String
    | ClosingParenthesisCodeGen
    | ConstructorCodeGen  String [CodeGenNode] [CodeGenNode] [CodeGenNode]
    | DataCodeGen  String [CodeGenNode]
    | DataElementCodeGen  String String [String] [String]
    | DataInstanceCodeGen  CodeGenNode [CodeGenNode]
    | DivideCodeGen
    | ElseCodeGen  [CodeGenNode]
    | ElseIfCodeGen  CodeGenNode [CodeGenNode]
    | Empty
    | ErrorCodeGen
    | ForCodeGen  String CodeGenNode CodeGenNode [CodeGenNode]
    | FunctionCodeGen  CodeGenNode (Maybe CodeGenNode) [CodeGenNode] CodeGenNode Bool [CodeGenNode]
    | FunctionCallCodeGen  String [CodeGenNode]
    | GlobalVarCodeGen  String Bool Bool CodeGenNode CodeGenNode [CodeGenNode]
    | GreaterEqualCodeGen
    | GreaterCodeGen
    | IdentifierCodeGen  String
    | IfCodeGen  CodeGenNode [CodeGenNode]
    | ImportCodeGen  [String]
    | LambdaCodeGen  String [CodeGenNode]
    | LessCodeGen
    | LessEqualCodeGen
    | IntConstCodeGen  Integer
    | MainFunctionCodeGen  CodeGenNode (Maybe CodeGenNode) [CodeGenNode] CodeGenNode [CodeGenNode]
    | MethodCallCodeGen  String [CodeGenNode]
    | ModifierBlockCodeGen  [CodeGenNode]
    | MultiplyCodeGen
    | NegCodeGen  CodeGenNode
    | NewClassInstanceCodeGen  CodeGenNode [CodeGenNode]
    | NotCodeGen  CodeGenNode
    | ObjectCodeGen  (Maybe CodeGenNode) String (Maybe CodeGenNode) [CodeGenNode] (Maybe String) [String] [CodeGenNode] [CodeGenNode] [CodeGenNode] [CodeGenNode]
    | ObjectMethodCallCodeGen  String String [CodeGenNode]
    | OpeningParenthesisCodeGen
    | OrCodeGen
    | PackageCodeGen  [String]
    | ParameterizedTypeCodeGen  CodeGenNode CodeGenNode
    | ParenthesisCodeGen  CodeGenNode
    | PrintCodeGen  CodeGenNode
    | ReassignCodeGen  CodeGenNode CodeGenNode
    | ReturnCodeGen  CodeGenNode
    | ReturnTypeCodeGen  String
    | RBinaryCodeGen  CodeGenNode CodeGenNode CodeGenNode
    | RBinOpErrorCodeGen
    | SeqCodeGen  [CodeGenNode]
    | SkipCodeGen
    | StringLiteralCodeGen  String
    | SubtractCodeGen
    | SuperCodeGen
    | SuperMethodCallCodeGen  String [CodeGenNode]
    | ThisCodeGen
    | ThisMethodCallCodeGen  String [CodeGenNode]
    | ThisVarCodeGen  CodeGenNode
    | TraitCodeGen  (Maybe CodeGenNode) String (Maybe CodeGenNode) [CodeGenNode] (Maybe String) [String] [CodeGenNode] [CodeGenNode] [CodeGenNode] [CodeGenNode]
    | TryCodeGen  [CodeGenNode]
    | TypeCodeGen  CodeGenNode
    | TypeParameterCodeGen  CodeGenNode
    | WhereCodeGen  [CodeGenNode]
    | WhileCodeGen  CodeGenNode [CodeGenNode]
        deriving (Eq, Show)

instance CodeGen CodeGenNode where
    genCode (ABinaryCodeGen aBinOp aExpr1 aExpr2) = return ()
    genCode (AddCodeGen ) = return ()
    genCode (AndCodeGen ) = return ()
    genCode (AnnotationCodeGen  name)  = return ()
    genCode (ArgumentCodeGen  b) = return ()
    genCode (ArgumentTypeCodeGen  b) = return ()
    genCode (ArithExprCodeGen  aExpr) = return ()
    genCode (ArrayAppendCodeGen  arrays)  = return ()
    genCode (ArrayElementSelectCodeGen  i)  = return ()
    genCode (ArrayValuesCodeGen  exprs)  = return ()
    genCode (AssignCodeGen  immutable vType name value)  = return ()
    genCode (AssignArithCodeGen  mutable vType name value)  = return ()
    genCode (BBinaryCodeGen   bbinop bExpr1 bExpr2) = return ()
    genCode (BoolConstCodeGen   b) = return ()
    genCode (BooleanExprCodeGen  expr)  = return ()
    genCode (CatchCodeGen  params exprs)  = return ()
    genCode (ClassCodeGen  package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) = return ()
    genCode (ClassVariableCodeGen  className varName)  = return ()
    genCode (ClosingParenthesisCodeGen ) = return ()
    genCode (ConstructorCodeGen  name argTypes args body)  = return ()
    genCode (DataCodeGen  name exprs)  = return ()
    genCode (DataElementCodeGen  superName name argTypes args)  = return ()
    genCode (DivideCodeGen )  = return ()
    genCode (ElseCodeGen  statement)  = return ()
    genCode (ElseIfCodeGen  condition statement)  = return ()
    genCode (ForCodeGen  varName start end exprs)  = return ()
    genCode (FunctionCodeGen  name annotations params returnType static body)  = return ()
    genCode (FunctionCallCodeGen  name exprs)  = return ()
    genCode (GlobalVarCodeGen  modifier final static varType varName exprs)  = return ()
    genCode (GreaterEqualCodeGen )  = return ()
    genCode (GreaterCodeGen )  = return ()
    genCode (IdentifierCodeGen  name)  = return ()
    genCode (IfCodeGen  condition statement)  = return ()
    genCode (ImportCodeGen  locs)  = return ()
    genCode (IntConstCodeGen  i)  = return ()
    genCode (LambdaCodeGen  varName exprs)  = return ()
    genCode (LessEqualCodeGen )  = return ()
    genCode (LessCodeGen )  = return ()
    genCode (MainFunctionCodeGen  name annotations params returnType body)  = return ()
    genCode (MethodCallCodeGen  methodName args)  = return ()
    genCode (ModifierBlockCodeGen  exprs)  = return ()
    genCode (MultiplyCodeGen )  = return ()
    genCode (NegCodeGen  aExpr)  = return ()
    genCode (NewClassInstanceCodeGen  className args)  = return ()
    genCode (NotCodeGen   n) = return ()
    genCode (ObjectCodeGen  package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) = return ()
    genCode (ObjectMethodCallCodeGen  objectName methodName args)  = return ()
    genCode (OrCodeGen) = return ()
    genCode (OpeningParenthesisCodeGen ) = return ()
    genCode (PackageCodeGen  locs)  = return ()
    genCode (ParameterizedTypeCodeGen  className typeName) = return ()
    genCode (ParameterCodeGen  varType varName)  = return ()
    genCode (ParenthesisCodeGen  aExpr)  = return ()
    genCode (PrintCodeGen  exprs)  = return ()
    genCode (RBinaryCodeGen   rbinop aExpr1 aExpr2) = return ()
    genCode (ReassignCodeGen  name value)  = return ()
    genCode (ReturnCodeGen  expr)  = return ()
    genCode (ReturnTypeCodeGen  b)  = return ()
    genCode (SeqCodeGen  s)  = return ()
    genCode (SkipCodeGen )  = return ()
    genCode (StringLiteralCodeGen  value)  = return ()
    genCode (SubtractCodeGen )  = return ()
    genCode (SuperCodeGen )  = return ()
    genCode (SuperMethodCallCodeGen  methodName args)  = return ()
    genCode (ThisCodeGen )  = return ()
    genCode (ThisMethodCallCodeGen  methodName args)  = return ()
    genCode (ThisVarCodeGen  varName)  = return ()
    genCode (TraitCodeGen  package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) = return ()
    genCode (TryCodeGen  exprs)  = return ()
    genCode (TypeCodeGen  b)  = return ()
    genCode (TypeParameterCodeGen  typeName)  = return ()
    genCode (WhereCodeGen  exprs)  = return ()
    genCode (WhileCodeGen  condition statement)  = return ()
    genCode (Empty )  = return ()
