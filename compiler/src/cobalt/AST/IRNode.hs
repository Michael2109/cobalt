{-|
Module      : IRNode
Description : Contains functions for working with intermediate representation tree.
-}
module AST.IRNode where

import Data.Char
import Data.List
import Data.Maybe
import Text.Format
import Text.PrettyPrint.Annotated.Leijen

import AST.CodeGenNode
import SymbolTable.SymbolTable
import Util.GeneralUtil

class CodeGenIR a where
    genCodeGenIR :: a -> CodeGenNode

data IRNode
    = ABinaryIR IRNode IRNode IRNode
    | ABinOpErrorIR
    | AddIR
    | AndIR
    | AErrorIR
    | AnnotationIR String
    | ArgumentIR IRNode
    | ArgumentTypeIR String
    | ArithExprIR IRNode
    | ArrayAppendIR [IRNode]
    | ArrayElementSelectIR String
    | ArrayValuesIR [String]
    | AssignIR Bool (Maybe IRNode) IRNode IRNode
    | AssignArithIR Bool IRNode String IRNode
    | BBinaryIR IRNode IRNode IRNode
    | BBinOpErrorIR
    | BErrorIR
    | BoolConstIR Bool
    | BooleanExprIR IRNode
    | CatchIR [IRNode] [IRNode]
    | ClassIR (Maybe IRNode) String (Maybe IRNode) [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
    | ParameterIR IRNode IRNode
    | ClassVariableIR String String
    | ClosingParenthesisIR
    | ConstructorIR String [IRNode] [IRNode] [IRNode]
    | DataIR String [IRNode]
    | DataElementIR String String [String] [String]
    | DataInstanceIR IRNode [IRNode]
    | DivideIR
    | ElseIR [IRNode]
    | ElseIfIR IRNode [IRNode]
    | ErrorIR
    | ForIR String IRNode IRNode [IRNode]
    | FunctionCallIR String [IRNode]
    | GlobalVarIR String Bool Bool IRNode IRNode [IRNode]
    | GreaterEqualIR
    | GreaterIR
    | IdentifierIR String
    | IfIR IRNode [IRNode]
    | ImportIR [String]
    | LambdaIR String [IRNode]
    | LessIR
    | LessEqualIR
    | IntConstIR Integer
    | MainFunctionIR IRNode (Maybe IRNode) [IRNode] IRNode [IRNode]
    | MethodCallIR String [IRNode]
    | MethodIR IRNode (Maybe IRNode) [IRNode] IRNode Bool [IRNode]
    | ModifierBlockIR [IRNode]
    | MultiplyIR
    | NegIR IRNode
    | NewClassInstanceIR IRNode [IRNode]
    | NotIR IRNode
    | ObjectIR (Maybe IRNode) String (Maybe IRNode) [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
    | ObjectMethodCallIR String String [IRNode]
    | OpeningParenthesisIR
    | OrIR
    | PackageIR [String]
    | ParameterizedTypeIR IRNode IRNode
    | ParenthesisIR IRNode
    | PrintIR IRNode
    | ReassignIR IRNode IRNode
    | ReturnIR IRNode
    | ReturnTypeIR String
    | RBinaryIR IRNode IRNode IRNode
    | RBinOpErrorIR
    | SeqIR [IRNode]
    | SkipIR
    | StringLiteralIR String
    | SubtractIR
    | SuperIR
    | SuperMethodCallIR String [IRNode]
    | ThisIR
    | ThisMethodCallIR String [IRNode]
    | ThisVarIR IRNode
    | TraitIR (Maybe IRNode) String (Maybe IRNode) [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
    | TryIR [IRNode]
    | TypeIR IRNode
    | TypeParameterIR IRNode
    | WhereIR [IRNode]
    | WhileIR IRNode [IRNode]
    deriving (Eq, Show)

instance CodeGenIR IRNode where
    genCodeGenIR (ABinaryIR aBinOp aExpr1 aExpr2)  = ABinaryCodeGen (genCodeGenIR aBinOp) (genCodeGenIR aExpr1) (genCodeGenIR aExpr2)
    genCodeGenIR (AddIR)  = AddCodeGen
    genCodeGenIR (AndIR)  = AndCodeGen
    genCodeGenIR (AnnotationIR name)  = AnnotationCodeGen  name
    genCodeGenIR (ArgumentIR a)  = ArgumentCodeGen  (genCodeGenIR a )
    genCodeGenIR (ArgumentTypeIR aType)  = ArgumentTypeCodeGen  aType
    genCodeGenIR (ArithExprIR aExpr)  = ArithExprCodeGen  (genCodeGenIR aExpr )
    genCodeGenIR (ArrayAppendIR arrays)  = ArrayAppendCodeGen  (irNodeArrToCodeGenNodeArray arrays )
    genCodeGenIR (ArrayElementSelectIR index)  = ArrayElementSelectCodeGen index
    genCodeGenIR (ArrayValuesIR exprs)  = ArrayValuesCodeGen  exprs
    genCodeGenIR (AssignIR immutable vType name value)  = AssignCodeGen  immutable (maybeIRNodeToMaybeCodeGenNode vType ) (genCodeGenIR name ) (genCodeGenIR value )
    genCodeGenIR (AssignArithIR mutable vType name value)  = AssignArithCodeGen  mutable (genCodeGenIR vType ) name (genCodeGenIR value )
    genCodeGenIR (BBinaryIR bbinop bExpr1 bExpr2)  = BBinaryCodeGen  (genCodeGenIR bbinop ) (genCodeGenIR bExpr1 ) (genCodeGenIR bExpr2 )
    genCodeGenIR (BoolConstIR b)  = BoolConstCodeGen  b
    genCodeGenIR (BooleanExprIR expr)  = BooleanExprCodeGen  (genCodeGenIR expr )
    genCodeGenIR (CatchIR params exprs)  = CatchCodeGen  (irNodeArrToCodeGenNodeArray params ) (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (ClassIR package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray)  = ClassCodeGen  (maybeIRNodeToMaybeCodeGenNode package ) name (maybeIRNodeToMaybeCodeGenNode typeParam ) (map (\a -> genCodeGenIR a ) params) parent interfaces (map (\a -> genCodeGenIR a ) imports) (map (\a -> genCodeGenIR a ) modifierBlocks) (map (\a -> genCodeGenIR a ) constructorExprs) (map (\a -> genCodeGenIR a ) bodyArray)
    genCodeGenIR (ClassVariableIR className varName)  = ClassVariableCodeGen  className varName
    genCodeGenIR (ClosingParenthesisIR) = ClosingParenthesisCodeGen
    genCodeGenIR (ConstructorIR name argTypes args exprs)  = ConstructorCodeGen  name (irNodeArrToCodeGenNodeArray argTypes ) (irNodeArrToCodeGenNodeArray args ) (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (DivideIR)  = DivideCodeGen
    genCodeGenIR (ElseIR exprs)  = ElseCodeGen  (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (ElseIfIR condition exprs)  = ElseIfCodeGen  (genCodeGenIR condition ) (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (ForIR varName start end exprs)  = ForCodeGen  varName (genCodeGenIR start ) (genCodeGenIR end ) (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (FunctionCallIR name exprs)  = FunctionCallCodeGen  name $ irNodeArrToCodeGenNodeArray exprs
    genCodeGenIR (GreaterEqualIR)  = GreaterEqualCodeGen
    genCodeGenIR (GreaterIR)  = GreaterCodeGen
    genCodeGenIR (GlobalVarIR modifier final static varType varName exprs)  = GlobalVarCodeGen  modifier final static (genCodeGenIR varType ) (genCodeGenIR varName )  (map (\e -> genCodeGenIR e ) exprs)
    genCodeGenIR (IdentifierIR name)  = IdentifierCodeGen  name
    genCodeGenIR (IfIR condition exprs)  = IfCodeGen  (genCodeGenIR condition ) $ irNodeArrToCodeGenNodeArray exprs 
    genCodeGenIR (ImportIR locs)  = ImportCodeGen  locs
    genCodeGenIR (IntConstIR i)  = IntConstCodeGen i
    genCodeGenIR (LessEqualIR)  = LessEqualCodeGen
    genCodeGenIR (LessIR)  = LessCodeGen
    genCodeGenIR (MainFunctionIR name annotations params returnType exprs)  = MainFunctionCodeGen  (genCodeGenIR name ) (maybeIRNodeToMaybeCodeGenNode annotations ) (irNodeArrToCodeGenNodeArray params ) (genCodeGenIR returnType ) (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (MethodCallIR methodName args)  = MethodCallCodeGen  methodName (irNodeArrToCodeGenNodeArray args )
    genCodeGenIR (MethodIR name annotations params returnType static exprs)  = MethodCodeGen  (genCodeGenIR name ) (maybeIRNodeToMaybeCodeGenNode annotations ) (irNodeArrToCodeGenNodeArray params ) (genCodeGenIR returnType ) static (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (ModifierBlockIR exprs)  = ModifierBlockCodeGen  (map (\e -> genCodeGenIR e ) exprs)
    genCodeGenIR (MultiplyIR)  = MultiplyCodeGen
    genCodeGenIR (NegIR aExpr)  = NegCodeGen (genCodeGenIR aExpr)
    genCodeGenIR (NewClassInstanceIR className args)  = NewClassInstanceCodeGen  (genCodeGenIR className ) $ irNodeArrToCodeGenNodeArray args 
    genCodeGenIR (NotIR n)  = NotCodeGen  $ genCodeGenIR n 
    genCodeGenIR (ObjectIR package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray)  = ObjectCodeGen  (maybeIRNodeToMaybeCodeGenNode package ) name (maybeIRNodeToMaybeCodeGenNode typeParam ) (map (\a -> genCodeGenIR a ) params) parent interfaces (map (\a -> genCodeGenIR a ) imports) (map (\a -> genCodeGenIR a ) modifierBlocks) (map (\a -> genCodeGenIR a ) constructorExprs) (map (\a -> genCodeGenIR a ) bodyArray)
    genCodeGenIR (ObjectMethodCallIR objectName methodName args)  = ObjectMethodCallCodeGen  objectName methodName (irNodeArrToCodeGenNodeArray args )
    genCodeGenIR (OpeningParenthesisIR) = OpeningParenthesisCodeGen
    genCodeGenIR (OrIR)  = OrCodeGen
    genCodeGenIR (PackageIR locs)  = PackageCodeGen  locs
    genCodeGenIR (ParameterizedTypeIR className typeName)  = ParameterizedTypeCodeGen  (genCodeGenIR className ) (genCodeGenIR typeName )
    genCodeGenIR (ParameterIR varType varName)  = ParameterCodeGen  (genCodeGenIR varType ) (genCodeGenIR varName )
    genCodeGenIR (ParenthesisIR aExpr)  = ParenthesisCodeGen (genCodeGenIR aExpr)
    genCodeGenIR (PrintIR expr)  = PrintCodeGen  $ genCodeGenIR expr 
    genCodeGenIR (ReassignIR name value)  = ReassignCodeGen  (genCodeGenIR name ) (genCodeGenIR value )
    genCodeGenIR (ReturnIR expr)  = ReturnCodeGen  (genCodeGenIR expr )
    genCodeGenIR (ReturnTypeIR returnType)  = ReturnTypeCodeGen  returnType
    genCodeGenIR (RBinaryIR rbinop aExpr1 aExpr2)  = RBinaryCodeGen  (genCodeGenIR rbinop ) (genCodeGenIR aExpr1 ) (genCodeGenIR aExpr2 )
    genCodeGenIR (SeqIR s)  = SeqCodeGen  (irNodeArrToCodeGenNodeArray s )
    genCodeGenIR (SkipIR)  = SkipCodeGen
    genCodeGenIR (StringLiteralIR value)  = StringLiteralCodeGen  value
    genCodeGenIR (SubtractIR)  = SubtractCodeGen
    genCodeGenIR (SuperIR)  = SuperCodeGen
    genCodeGenIR (SuperMethodCallIR methodName args)  = SuperMethodCallCodeGen  methodName (irNodeArrToCodeGenNodeArray args )
    genCodeGenIR (ThisIR)  = ThisCodeGen
    genCodeGenIR (ThisMethodCallIR methodName args)  = ThisMethodCallCodeGen  methodName (irNodeArrToCodeGenNodeArray args )
    genCodeGenIR (ThisVarIR varName)  = ThisVarCodeGen  $ genCodeGenIR varName 
    genCodeGenIR (TraitIR package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray)  = TraitCodeGen  (maybeIRNodeToMaybeCodeGenNode package ) name (maybeIRNodeToMaybeCodeGenNode typeParam ) (map (\a -> genCodeGenIR a ) params) parent interfaces (map (\a -> genCodeGenIR a ) imports) (map (\a -> genCodeGenIR a ) modifierBlocks) (map (\a -> genCodeGenIR a ) constructorExprs) (map (\a -> genCodeGenIR a ) bodyArray)
    genCodeGenIR (TryIR exprs)  = TryCodeGen  (irNodeArrToCodeGenNodeArray exprs )
    genCodeGenIR (TypeIR b)  = TypeCodeGen  (genCodeGenIR b )
    genCodeGenIR (TypeParameterIR typeName)  = TypeParameterCodeGen  (genCodeGenIR typeName )
    genCodeGenIR (WhereIR exprs)  = WhereCodeGen  $ irNodeArrToCodeGenNodeArray exprs 
    genCodeGenIR (WhileIR condition exprs)  = WhileCodeGen  (genCodeGenIR condition ) $ irNodeArrToCodeGenNodeArray exprs 

maybeIRNodeToMaybeCodeGenNode :: Maybe IRNode -> Maybe CodeGenNode
maybeIRNodeToMaybeCodeGenNode mExpr  =
    case mExpr of
        Just e -> Just $ genCodeGenIR e 
        Nothing -> Nothing

irNodeArrToCodeGenNodeArray :: [IRNode] -> [CodeGenNode]
irNodeArrToCodeGenNodeArray exprs  = (map (\a -> genCodeGenIR a ) exprs)
