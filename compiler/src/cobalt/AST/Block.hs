{-|
Module      : Block
Description : Data types that store general expressions.
These are used to store the data and be converted into IRNodes
-}
module AST.Block where

import Data.Char
import Data.List
import Data.Scientific
import Text.Show.Functions

import AST.IRNode
import AST.Modifier
import SymbolTable.SymbolTable

class ErrorCheck a where
    errorCheck :: a -> String

class SymbolTableGen a where
    genClassSymbolTable :: a -> ClassSymbolTable

class IRGen a where
    genIR :: a -> SymbolTable -> CurrentState -> IRNode

data Expr
    = ABinary Expr Expr Expr
    | ABinOpError
    | Add
    | AError
    | And
    | Annotation String
    | Argument Expr
    | ArgumentType String
    | ArithExpr Expr
    | ArrayAppend [Expr]
    | ArrayElementSelect String
    | ArrayValues [String]
    | Assign Bool (Maybe Expr) Expr Expr
    | AssignArith Bool Expr String Expr
    | BBinary Expr Expr Expr
    | BBinOpError
    | BError
    | BoolConst Bool
    | BooleanExpr Expr
    | Catch [Expr] [Expr]
    | Class (Maybe Expr) String (Maybe Expr) [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
    | ClassVariable String String
    | ClosingParenthesis
    | Constructor String [Expr] [Expr] [Expr]
    | Divide
    | DoubleConst Scientific
    | Else [Expr]
    | ElseIf Expr [Expr]
    | Error
    | For String Expr Expr [Expr]
    | FunctionCall String [Expr]
    | GlobalVar String Bool Bool Expr Expr [Expr]
    | Greater
    | GreaterEqual
    | Identifier String
    | If Expr [Expr]
    | Import [String]
    | IntConst Integer
    | Less
    | LessEqual
    | MainFunction Expr (Maybe Expr) [Expr] Expr [Expr]
    | MethodCall String [Expr]
    | Method Expr (Maybe Expr) [Modifier] [Expr] Expr Bool [Expr]
    | ModifierBlock [Expr]
    | Multiply
    | Neg Expr
    | NewClassInstance Expr [Expr]
    | Not Expr
    | Object (Maybe Expr) String (Maybe Expr) [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
    | ObjectMethodCall String String [Expr]
    | OpeningParenthesis
    | Or
    | Package [String]
    | ParameterizedType Expr Expr
    | Parameter Expr Expr
    | Parenthesis Expr
    | Print Expr
    | RBinary Expr Expr Expr
    | Reassign Expr Expr
    | Return Expr
    | ReturnType String
    | RBinOpError
    | Seq [Expr]
    | Skip
    | StringLiteral String
    | Subtract
    | Super
    | SuperMethodCall String [Expr]
    | This
    | ThisMethodCall String [Expr]
    | ThisVar Expr
    | TypeParameter Expr
    | Trait (Maybe Expr) String (Maybe Expr) [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
    | Try [Expr]
    | Type Expr
    | Where [Expr]
    | While Expr [Expr]
  deriving (Eq, Show)

instance SymbolTableGen Expr where
    genClassSymbolTable (Class packageLocs name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ClassType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
    --genClassSymbolTable (ClassParam varType varName) = (ClassSymbolTable "" NoType [(show varName, show varType)] [])
    --genClassSymbolTable (Function name annotations argTypes args returnType static body) = ClassSymbolTable "" NoType [] [(name, (MethodSymbolTable (show returnType) (zip (map show args) (map show argTypes))))]
    --genClassSymbolTable (GlobalVar modifier final static varType varName exprs) = (ClassSymbolTable "" NoType [(show varName, show varType)] [])
    genClassSymbolTable (ModifierBlock exprs) =  foldl1 (\x y -> combineClassSymbolTable x y) (map genClassSymbolTable exprs)
    genClassSymbolTable (Object packageLocs name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) =  combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ClassType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
    genClassSymbolTable (Trait packageLocs name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) =  combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ClassType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
    genClassSymbolTable (_) = ClassSymbolTable "" NoType [] []

instance IRGen Expr where
    genIR (ABinary aBinOp aExpr1 aExpr2) st cs = ABinaryIR (genIR aBinOp st cs) (genIR aExpr1 st cs) (genIR aExpr2 st cs)
    genIR (Add) st cs = AddIR
    genIR (And) st cs = AndIR
    genIR (Annotation name) st cs = AnnotationIR name
    genIR (Argument a) st cs = ArgumentIR (genIR a st cs)
    genIR (ArgumentType aType) st cs = ArgumentTypeIR  aType
    genIR (ArithExpr aExpr) st cs = ArithExprIR (genIR aExpr st cs)
    genIR (ArrayAppend arrays) st cs = ArrayAppendIR (exprArrToIRArray arrays st cs)
    genIR (ArrayElementSelect index) st cs = ArrayElementSelectIR index
    genIR (ArrayValues exprs) st cs = ArrayValuesIR exprs
    genIR (Assign immutable vType name value) st cs = AssignIR  immutable (maybeExprToMaybeIRNode vType st cs) (genIR name st cs) (genIR value st cs)
    genIR (AssignArith mutable vType name value) st cs = AssignArithIR  mutable (genIR vType st cs) name (genIR value st cs)
    genIR (BBinary bbinop bExpr1 bExpr2) st cs = BBinaryIR  (genIR bbinop st cs) (genIR bExpr1 st cs) (genIR bExpr2 st cs)
    genIR (BoolConst b) st cs = BoolConstIR  b
    genIR (BooleanExpr expr) st cs = BooleanExprIR  (genIR expr st cs)
    genIR (Catch params exprs) st cs = CatchIR  (exprArrToIRArray params st cs) (exprArrToIRArray exprs st cs)
    genIR (Class package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = ClassIR  (maybeExprToMaybeIRNode package st cs) name (maybeExprToMaybeIRNode typeParam st cs) (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (ClassVariable className varName) st cs = ClassVariableIR  className varName
    genIR (ClosingParenthesis) st cs = ClosingParenthesisIR
    genIR (Constructor name argTypes args exprs) st cs = ConstructorIR  name (exprArrToIRArray argTypes st cs) (exprArrToIRArray args st cs) (exprArrToIRArray exprs st cs)
    genIR (Divide) st cs = DivideIR
    genIR (Else exprs) st cs = ElseIR  (exprArrToIRArray exprs st cs)
    genIR (ElseIf condition exprs) st cs = ElseIfIR  (genIR condition st cs) (exprArrToIRArray exprs st cs)
    genIR (For varName start end exprs) st cs = ForIR  varName (genIR start st cs) (genIR end st cs) (exprArrToIRArray exprs st cs)
    genIR (FunctionCall name exprs) st cs = FunctionCallIR  name $ exprArrToIRArray exprs st cs
    genIR (GreaterEqual) symbolTable currentState = GreaterEqualIR 
    genIR (Greater) symbolTable currentState = GreaterIR 
    genIR (GlobalVar modifier final static varType varName exprs) st cs = GlobalVarIR  modifier final static (genIR varType st cs) (genIR varName st cs)  (map (\e -> genIR e st cs) exprs)
    genIR (Identifier name) st cs = IdentifierIR  name
    genIR (If condition exprs) st cs = IfIR  (genIR condition st cs) $ exprArrToIRArray exprs st cs
    genIR (Import locs) st cs = ImportIR  locs
    genIR (IntConst i) st cs = IntConstIR i
    genIR (LessEqual) st cs = LessEqualIR
    genIR (Less) st cs = LessIR
    genIR (MainFunction name annotations params returnType exprs) st cs = MainFunctionIR  (genIR name st cs) (maybeExprToMaybeIRNode annotations st cs) (exprArrToIRArray params st cs) (genIR returnType st cs) (exprArrToIRArray exprs st cs)
    genIR (MethodCall methodName args) st cs = MethodCallIR  methodName (exprArrToIRArray args st cs)
    genIR (Method name annotations modifiers params returnType static exprs) st cs = MethodIR  (genIR name st cs) (maybeExprToMaybeIRNode annotations st cs) modifiers (exprArrToIRArray params st cs) (genIR returnType st cs) static (exprArrToIRArray exprs st cs)
    genIR (ModifierBlock exprs) st cs = ModifierBlockIR  (map (\e -> genIR e st cs) exprs)
    genIR (Multiply) st cs = MultiplyIR
    genIR (Neg aExpr) st cs = NegIR (genIR aExpr st cs)
    genIR (NewClassInstance className args) st cs = NewClassInstanceIR  (genIR className st cs) $ exprArrToIRArray args st cs
    genIR (Not n) st cs = NotIR  $ genIR n st cs
    genIR (Object package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = ObjectIR  (maybeExprToMaybeIRNode package st cs) name (maybeExprToMaybeIRNode typeParam st cs) (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (ObjectMethodCall objectName methodName args) st cs = ObjectMethodCallIR  objectName methodName (exprArrToIRArray args st cs)
    genIR (OpeningParenthesis) st cs = OpeningParenthesisIR
    genIR (Or) st cs = OrIR 
    genIR (Package locs) st cs = PackageIR  locs
    genIR (ParameterizedType className typeName) st cs = ParameterizedTypeIR  (genIR className st cs) (genIR typeName st cs)
    genIR (Parameter varType varName) st cs = ParameterIR  (genIR varType st cs) (genIR varName st cs)
    genIR (Parenthesis aExpr) st cs = ParenthesisIR (genIR aExpr st cs)
    genIR (Print expr) st cs = PrintIR  $ genIR expr st cs
    genIR (Reassign name value) st cs = ReassignIR  (genIR name st cs) (genIR value st cs)
    genIR (Return expr) st cs = ReturnIR  (genIR expr st cs)
    genIR (ReturnType returnType) st cs = ReturnTypeIR  returnType
    genIR (RBinary rbinop aExpr1 aExpr2) st cs = RBinaryIR  (genIR rbinop st cs) (genIR aExpr1 st cs) (genIR aExpr2 st cs)
    genIR (Seq s) st cs = SeqIR  (exprArrToIRArray s st cs)
    genIR (Skip) st cs = SkipIR 
    genIR (StringLiteral value) st cs = StringLiteralIR  value
    genIR (Subtract) st cs = SubtractIR 
    genIR (Super) st cs = SuperIR 
    genIR (SuperMethodCall methodName args) st cs = SuperMethodCallIR  methodName (exprArrToIRArray args st cs)
    genIR (This) st cs = ThisIR 
    genIR (ThisMethodCall methodName args) st cs = ThisMethodCallIR  methodName (exprArrToIRArray args st cs)
    genIR (ThisVar varName) st cs = ThisVarIR  $ genIR varName st cs
    genIR (Trait package name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = TraitIR  (maybeExprToMaybeIRNode package st cs) name (maybeExprToMaybeIRNode typeParam st cs) (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (Try exprs) st cs = TryIR  (exprArrToIRArray exprs st cs)
    genIR (Type b) st cs = TypeIR  (genIR b st cs)
    genIR (TypeParameter typeName) st cs = TypeParameterIR  (genIR typeName st cs)
    genIR (Where exprs) st cs = WhereIR  $ exprArrToIRArray exprs st cs
    genIR (While condition exprs) st cs = WhileIR  (genIR condition st cs) $ exprArrToIRArray exprs st cs

maybeExprToMaybeIRNode :: Maybe Expr -> SymbolTable -> CurrentState -> Maybe IRNode
maybeExprToMaybeIRNode mExpr st cs =
    case mExpr of
        Just e -> Just $ genIR e st cs
        Nothing -> Nothing

exprArrToIRArray :: [Expr] -> SymbolTable -> CurrentState -> [IRNode]
exprArrToIRArray exprs st cs = (map (\a -> genIR a st cs) exprs)
