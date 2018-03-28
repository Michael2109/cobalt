{-|
Module      : Block
Description : Data types that store general expressions.
These are used to store the data and be converted into IRNodes
-}
module Block where

import Data.List
import Text.Show.Functions
import Data.Char
import Data.Scientific

import SymbolTable
import IRNode


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
  | ArrayAssignment Expr Expr
  | ArrayDef String String
  | ArrayElementSelect String
  | ArrayType String
  | ArrayValues [String]
  | Assign Expr Expr Expr
  | AssignArith Bool Expr String Expr
  | BBinary Expr Expr Expr
  | BBinOpError
  | BError
  | BoolConst Bool
  | BooleanExpr Expr
  | Catch [Expr] [Expr]
  | Class [String] String (Maybe Expr) [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ClassParam Expr Expr
  | ClassVariable String String
  | ClosingParenthesis
  | Constructor String [Expr] [Expr] [Expr]
  | Divide
  | DoubleConst Scientific
  | Else [Expr]
  | ElseIf Expr [Expr]
  | Error
  | For String Expr Expr [Expr]
  | Function Expr (Maybe Expr) [Expr] Expr Bool [Expr]
  | FunctionCall String [Expr]
  | GlobalVar String Bool Bool Expr Expr [Expr]
  | Greater
  | GreaterEqual
  | Identifier String
  | If Expr [Expr]
  | Import {locs ::[String]}
  | IntConst Integer
  | Less
  | LessEqual
  | MainFunction Expr (Maybe Expr) [Expr] Expr [Expr]
  | ModifierBlock [Expr]
  | Multiply
  | Neg Expr
  | NewClassInstance String [Expr]
  | Not Expr
  | Object [String] String (Maybe Expr) [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ObjectMethodCall String String [Expr]
  | OpeningParenthesis
  | Or
  | ParameterizedType Expr Expr
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
  | Trait [String] String (Maybe Expr) [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
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

    genIR (ABinary aBinOp aExpr1 aExpr2) symbolTable currentState = Empty (IRInfo $ "ABinary")
    genIR (Add) symbolTable currentState = Empty (IRInfo $ "Add")
    genIR (And) symbolTable currentState = AndIR (IRInfo $ "And")
    genIR (Annotation name) st cs = AnnotationIR (IRInfo $ "Annotation") name
    genIR (Argument a) st cs = ArgumentIR (IRInfo $ "Argument") (genIR a st cs)
    genIR (ArgumentType aType) st cs = ArgumentTypeIR (IRInfo $ "ArgumentType") aType
    genIR (ArithExpr aExpr) st cs = ArithExprIR (IRInfo $ "ArithExpr") (genIR aExpr st cs)
    genIR (ArrayAppend arrays) st cs = ArrayAppendIR (IRInfo $ "ArrayAppend") (exprArrToIRArray arrays st cs)
    genIR (ArrayAssignment arr values) st cs = ArrayAssignmentIR (IRInfo $ "ArrayAssignment") (genIR arr st cs) (genIR values st cs)
    genIR (ArrayDef arrType name) st cs = ArrayDefIR (IRInfo $ "ArrayDef") arrType name
    genIR (ArrayElementSelect index) st cs = ArrayElementSelectIR (IRInfo $ "Array Element Select") index
    genIR (ArrayType arrType) st cs = ArrayTypeIR (IRInfo $ "ArrayType") arrType
    genIR (ArrayValues exprs) st cs = ArrayValuesIR (IRInfo $ "ArrayValues") exprs
    genIR (Assign vType name value) st cs = AssignIR (IRInfo $ "Assign") (genIR vType st cs) (genIR name st cs) (genIR value st cs)
    genIR (AssignArith mutable vType name value) st cs = AssignArithIR (IRInfo $ "AssignArith") mutable (genIR vType st cs) name (genIR value st cs)
    genIR (BBinary bbinop bExpr1 bExpr2) st cs = BBinaryIR (IRInfo $ "BBinaryIR") (genIR bbinop st cs) (genIR bExpr1 st cs) (genIR bExpr2 st cs)
    genIR (BoolConst b) st cs = BoolConstIR (IRInfo $ "BoolConst") b
    genIR (BooleanExpr expr) st cs = BooleanExprIR (IRInfo $ "BooleanExpr") (genIR expr st cs)
    genIR (Catch params exprs) st cs = CatchIR (IRInfo $ "Catch") (exprArrToIRArray params st cs) (exprArrToIRArray exprs st cs)
    genIR (Class packageLocs name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = ClassIR (IRInfo $ "Class") packageLocs name (maybeExprToMaybeIRNode typeParam st cs) (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (ClassParam varType varName) st cs = ClassParamIR (IRInfo $ "ClassParam") (genIR varType st cs) (genIR varName st cs)
    genIR (ClassVariable className varName) st cs = ClassVariableIR (IRInfo $ "ClassVariable") className varName
    genIR (ClosingParenthesis) currentState symbolTable = Empty (IRInfo $ "ClosingParenthesis")
    genIR (Constructor name argTypes args exprs) st cs = ConstructorIR (IRInfo $ "Constructor") name (exprArrToIRArray argTypes st cs) (exprArrToIRArray args st cs) (exprArrToIRArray exprs st cs)
    genIR (Divide) symbolTable currentState = Empty (IRInfo $ "Divide")
    genIR (Else exprs) st cs = ElseIR (IRInfo $ "Else") (exprArrToIRArray exprs st cs)
    genIR (ElseIf condition exprs) st cs = ElseIfIR (IRInfo $ "ElseIf") (genIR condition st cs) (exprArrToIRArray exprs st cs)
    genIR (For varName start end exprs) st cs = ForIR (IRInfo $ "For") varName (genIR start st cs) (genIR end st cs) (exprArrToIRArray exprs st cs)
    genIR (Function name annotations params returnType static exprs) st cs = FunctionIR (IRInfo $ "Function") (genIR name st cs) (maybeExprToMaybeIRNode annotations st cs) (exprArrToIRArray params st cs) (genIR returnType st cs) static (exprArrToIRArray exprs st cs)
    genIR (FunctionCall name exprs) st cs = FunctionCallIR (IRInfo $ "FunctionCall") name $ exprArrToIRArray exprs st cs
    genIR (GreaterEqual) symbolTable currentState = GreaterEqualIR (IRInfo $ "GreaterEqual")
    genIR (Greater) symbolTable currentState = GreaterIR (IRInfo $ "Greater")
    genIR (GlobalVar modifier final static varType varName exprs) st cs = GlobalVarIR (IRInfo $ "GlobalVar") modifier final static (genIR varType st cs) (genIR varName st cs)  (map (\e -> genIR e st cs) exprs)
    genIR (Identifier name) st cs = IdentifierIR (IRInfo $ "Identifier") name
    genIR (If condition exprs) st cs = IfIR (IRInfo $ "If") (genIR condition st cs) $ exprArrToIRArray exprs st cs
    genIR (Import locs) st cs = ImportIR (IRInfo $ "Import") locs
    genIR (IntConst i) symbolTable currentState = Empty (IRInfo $ "IntConst")
    genIR (LessEqual) symbolTable currentState = LessEqualIR (IRInfo $ "LessEqual")
    genIR (Less) symbolTable currentState = LessIR (IRInfo $ "Less")
    genIR (MainFunction name annotations params returnType exprs) st cs = MainFunctionIR (IRInfo $ "MainFunction") (genIR name st cs) (maybeExprToMaybeIRNode annotations st cs) (exprArrToIRArray params st cs) (genIR returnType st cs) (exprArrToIRArray exprs st cs)
    genIR (ModifierBlock exprs) st cs = ModifierBlockIR (IRInfo $ "ModifierBlock") (map (\e -> genIR e st cs) exprs)
    genIR (Multiply) symbolTable currentState = Empty (IRInfo $ "Multiply")
    genIR (Neg aExpr) symbolTable currentState = Empty (IRInfo $ "Neg")
    genIR (NewClassInstance className args) st cs = NewClassInstanceIR (IRInfo $ "NewClassInstance") className $ exprArrToIRArray args st cs
    genIR (Not n) st cs = NotIR (IRInfo $ "Not") $ genIR n st cs
    genIR (Object packageLocs name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = ObjectIR (IRInfo $ "Object") packageLocs name (maybeExprToMaybeIRNode typeParam st cs) (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (ObjectMethodCall objectName methodName args) st cs = ObjectMethodCallIR (IRInfo $ "ObjectMethodCall") objectName methodName (exprArrToIRArray args st cs)
    genIR (OpeningParenthesis) currentState symbolTable = Empty (IRInfo $ "OpeningParenthesis")
    genIR (Or) st cs = OrIR (IRInfo $ "Or")
    genIR (ParameterizedType className typeName) st cs = ParameterizedTypeIR (IRInfo $ "ParameterizedType") (genIR className st cs) (genIR typeName st cs)
    genIR (Parenthesis aExpr) st cs = Empty (IRInfo $ "Parenthesis")
    genIR (Print expr) st cs = PrintIR (IRInfo $ "Print") $ genIR expr st cs
    genIR (Reassign name value) st cs = ReassignIR (IRInfo $ "Reassign") (genIR name st cs) (genIR value st cs)
    genIR (Return expr) st cs = ReturnIR (IRInfo $ "Return") (genIR expr st cs)
    genIR (ReturnType returnType) st cs = ReturnTypeIR (IRInfo $ "ReturnType") returnType
    genIR (RBinary rbinop aExpr1 aExpr2) st cs = RBinaryIR (IRInfo $ "RBinaryIR") (genIR rbinop st cs) (genIR aExpr1 st cs) (genIR aExpr2 st cs)
    genIR (Seq s) st cs = SeqIR (IRInfo $ "Seq") (exprArrToIRArray s st cs)
    genIR (Skip) st cs = SkipIR (IRInfo $ "Skip")
    genIR (StringLiteral value) st cs = StringLiteralIR (IRInfo $ "StringLiteral") value
    genIR (Subtract) st cs = SubtractIR (IRInfo $ "Subtract")
    genIR (Super) st cs = SuperIR (IRInfo $ "Super")
    genIR (SuperMethodCall methodName args) st cs = SuperMethodCallIR (IRInfo $ "SuperMethodCall") methodName (exprArrToIRArray args st cs)
    genIR (This) st cs = ThisIR (IRInfo $ "This")
    genIR (ThisMethodCall methodName args) st cs = ThisMethodCallIR (IRInfo $ "ObjectMethodCall") methodName (exprArrToIRArray args st cs)
    genIR (ThisVar varName) st cs = ThisVarIR (IRInfo $ "ThisVar") $ genIR varName st cs
    genIR (Trait packageLocs name typeParam params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = TraitIR (IRInfo $ "Trait") packageLocs name (maybeExprToMaybeIRNode typeParam st cs) (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (Try exprs) st cs = TryIR (IRInfo $ "Try") (exprArrToIRArray exprs st cs)
    genIR (Type b) st cs = TypeIR (IRInfo $ "Type") (genIR b st cs)
    genIR (TypeParameter typeName) st cs = TypeParameterIR (IRInfo $ "TypeParameter") (genIR typeName st cs)
    genIR (Where exprs) st cs = WhereIR (IRInfo $ "Where") $ exprArrToIRArray exprs st cs
    genIR (While condition exprs) st cs = WhileIR (IRInfo $ "While") (genIR condition st cs) $ exprArrToIRArray exprs st cs

maybeExprToMaybeIRNode :: Maybe Expr -> SymbolTable -> CurrentState -> Maybe IRNode
maybeExprToMaybeIRNode mExpr st cs = case mExpr of
                                       Just e -> Just $ genIR e st cs
                                       Nothing -> Nothing

exprArrToIRArray :: [Expr] -> SymbolTable -> CurrentState -> [IRNode]
exprArrToIRArray exprs st cs = (map (\a -> genIR a st cs) exprs)