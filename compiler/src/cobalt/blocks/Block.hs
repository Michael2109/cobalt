{-|
Module      : Block
Description : Data types that store general expressions.
These are used to store the data and specify how the code is generated.
-}

module Block where

import Data.List
import Text.Show.Functions
import Data.Char


import SymbolTable
import IRNode











class Debug a where
  debug :: a -> String

class ErrorCheck a where
  errorCheck :: a -> String

class SymbolTableGen a where
  genClassSymbolTable :: a -> ClassSymbolTable

class IRGen a where
  genIR :: a -> SymbolTable -> CurrentState -> IRNode

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr
  | AError
  deriving (Eq)

instance IRGen AExpr where
    genIR (Var v) symbolTable currentState = do
      -- todo
      --let methodList = map (snd) (filter (\x -> fst x == (currentMethodName currentState)) (methods symbolTable))
      --if(length methodList > 0)
      --then if (elem v (map fst (methodArgs (methodList!!0)))) then v else (v ++ "()")
      --else v
      Empty
    genIR (IntConst i) symbolTable currentState = Empty
    genIR (Neg aExpr) symbolTable currentState = Empty
    genIR (ABinary aBinOp aExpr1 aExpr2) symbolTable currentState = Empty
    genIR (Parenthesis aExpr) symbolTable currentState = Empty

instance Show AExpr where
  show (Var v) = v
  show (IntConst i) = show i
  show (Neg aExpr) = show aExpr
  show (ABinary aBinOp aExpr1 aExpr2) = show aBinOp ++ show aExpr1 ++ show aExpr2
  show (Parenthesis aExpr) = show aExpr
  show (AError) = "<AError>"

-- Arithmetic ops
data ABinOp
  = OpeningParenthesis
  | ClosingParenthesis
  | Add
  | Subtract
  | Multiply
  | Divide
  | ABinOpError
  deriving (Eq)

instance IRGen ABinOp where
    genIR (Add) symbolTable currentState = Empty
    genIR (Subtract) symbolTable currentState = Empty
    genIR (Multiply) symbolTable currentState = Empty
    genIR (Divide) symbolTable currentState = Empty
    genIR (OpeningParenthesis) currentState symbolTable = Empty
    genIR (ClosingParenthesis) currentState symbolTable = Empty

instance Show ABinOp where
  show (Add) = "+"
  show (Subtract) = "-"
  show (Multiply) = "*"
  show (Divide) = "/"
  show (OpeningParenthesis) = "("
  show (ClosingParenthesis) = ")"
  show (ABinOpError) = "<ABinOpError>"

-- Boolean expressions
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  | BError
  deriving (Eq)

instance IRGen BExpr where
    genIR (BoolConst b) symbolTable currentState = Empty
    genIR (Not n) symbolTable currentState = Empty
    genIR (BBinary bbinop bExpr1 bExpr2) symbolTable currentState = Empty
    genIR (RBinary rbinop aExpr1 aExpr2) symbolTable currentState = Empty

instance Show BExpr where
  show (BoolConst v) = if v then "true" else "false"
  show (RBinary rbinop aExpr1 aExpr2) = show rbinop ++ show aExpr1 ++ show aExpr2
  show (BError) = "<BError>"
  show (_) = "<UNKNOWN SHOW BEXPR>"

-- Boolean ops
data BBinOp
  = And
  | Or
  | BBinOpError
  deriving (Eq)

instance IRGen BBinOp where
    genIR (And) symbolTable currentState = Empty
    genIR (Or) symbolTable currentState = Empty

-- R binary ops
data RBinOp
  = Greater
  | GreaterEqual
  | Less
  | LessEqual
  | RBinOpError
  deriving (Eq)

instance IRGen RBinOp where
    genIR (Greater) symbolTable currentState = Empty
    genIR (Less) symbolTable currentState = Empty
    genIR (GreaterEqual) symbolTable currentState = Empty
    genIR (LessEqual) symbolTable currentState = Empty

instance Show RBinOp where
  show (Greater) = ">"
  show (Less) = "<"
  show (GreaterEqual) = ">="
  show (LessEqual) = "<="
  show (RBinOpError) = "<RBinOpError>"



data Expr
  = Annotation String
  | Argument Expr
  | ArgumentType String
  | ArithExpr AExpr
  | ArrayAppend [Expr]
  | ArrayAssignment Expr Expr
  | ArrayDef String String
  | ArrayElementSelect String
  | ArrayType String
  | ArrayValues [String]
  | Assign Expr Expr Expr
  | AssignArith Bool Expr String Expr
  | BooleanExpr BExpr
  | Catch [Expr] [Expr]
  | Class [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ClassParam Expr Expr
  | ClassVariable String String
  | Constructor String [Expr] [Expr] [Expr]
  | Else [Expr]
  | ElseIf Expr [Expr]
  | Error
  | For String Expr Expr [Expr]
  | Function String (Maybe Expr) [Expr] [Expr] Expr Bool [Expr]
  | FunctionCall String [Expr]
  | GlobalVar String Bool Bool Expr Expr [Expr]
  | Identifier String
  | If Expr [Expr]
  | Import {locs ::[String]}
  | MainFunction {name ::String, annotations :: (Maybe Expr), argTypes:: [Expr], args::[Expr], returnType::Expr, body::[Expr]}
  | ModifierBlock [Expr]
  | NewClassInstance String [Expr]
  | Object [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ObjectMethodCall String String [Expr]
  | Print Expr
  | Reassign Expr Expr
  | Return Expr
  | ReturnType String
  | Seq [Expr]
  | Skip
  | StringLiteral String
  | Super
  | SuperMethodCall String String [Expr]
  | This
  | ThisMethodCall String [Expr]
  | ThisVar Expr
  | Trait [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | Try [Expr]
  | Type Expr
  | Where [Expr]
  | While Expr [Expr]
  deriving (Eq, Show)


instance SymbolTableGen Expr where
  genClassSymbolTable (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ClassType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
  --genClassSymbolTable (ClassParam varType varName) = (ClassSymbolTable "" NoType [(show varName, show varType)] [])
  --genClassSymbolTable (Function name annotations argTypes args returnType static body) = ClassSymbolTable "" NoType [] [(name, (MethodSymbolTable (show returnType) (zip (map show args) (map show argTypes))))]
  --genClassSymbolTable (GlobalVar modifier final static varType varName exprs) = (ClassSymbolTable "" NoType [(show varName, show varType)] [])
  genClassSymbolTable (ModifierBlock exprs) =  foldl1 (\x y -> combineClassSymbolTable x y) (map genClassSymbolTable exprs)
  genClassSymbolTable (Object packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ObjectType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
  genClassSymbolTable (Trait packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name TraitType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
  genClassSymbolTable (_) = ClassSymbolTable "" NoType [] []


instance IRGen Expr where
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
    genIR (BooleanExpr expr) st cs = BooleanExprIR (IRInfo $ "BooleanExpr") (genIR expr st cs)
    genIR (Catch params exprs) st cs = CatchIR (IRInfo $ "Catch") (exprArrToIRArray params st cs) (exprArrToIRArray exprs st cs)
    genIR (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) st cs = ClassIR (IRInfo $ "Class") packageLocs name (map (\a -> genIR a st cs) params) parent interfaces (map (\a -> genIR a st cs) imports) (map (\a -> genIR a st cs) modifierBlocks) (map (\a -> genIR a st cs) constructorExprs) (map (\a -> genIR a st cs) bodyArray)
    genIR (ClassParam varType varName) st cs = ClassParamIR (IRInfo $ "ClassParam") (genIR varType st cs) (genIR varName st cs)
    genIR (ClassVariable className varName) st cs = ClassVariableIR (IRInfo $ "ClassVariable") className varName
    genIR (Constructor name argTypes args exprs) st cs = ConstructorIR (IRInfo $ "Constructor") name (exprArrToIRArray argTypes st cs) (exprArrToIRArray args st cs) (exprArrToIRArray exprs st cs)
    genIR (Else exprs) st cs = ElseIR (IRInfo $ "Else") (exprArrToIRArray exprs st cs)
    genIR (ElseIf condition exprs) st cs = ElseIfIR (IRInfo $ "ElseIf") (genIR condition st cs) (exprArrToIRArray exprs st cs)
    genIR (For varName start end exprs) st cs = ForIR (IRInfo $ "For") varName (genIR start st cs) (genIR end st cs) (exprArrToIRArray exprs st cs)
    genIR (Function name annotations argTypes args returnType static exprs) st cs = FunctionIR (IRInfo $ "Function") name (maybeExprToMaybeIRNode annotations st cs) (exprArrToIRArray argTypes st cs) (exprArrToIRArray args st cs) (genIR returnType st cs) static (exprArrToIRArray exprs st cs)
    genIR (FunctionCall name exprs) st cs = FunctionCallIR (IRInfo $ "FunctionCall") name $ exprArrToIRArray exprs st cs
    genIR (GlobalVar modifier final static varType varName exprs) st cs = GlobalVarIR (IRInfo $ "GlobalVar") modifier final static (genIR varType st cs) (genIR varName st cs)  (map (\e -> genIR e st cs) exprs)
    genIR (Identifier name) st cs = IdentifierIR (IRInfo $ "Identifier") name
    genIR (If condition exprs) st cs = IfIR (IRInfo $ "If") (genIR condition st cs) $ exprArrToIRArray exprs st cs
    genIR (Import locs) st cs = ImportIR (IRInfo $ "Import") locs
    genIR (MainFunction name annotations argTypes args returnType exprs) st cs = MainFunctionIR (IRInfo $ "MainFunction") name (maybeExprToMaybeIRNode annotations st cs) (exprArrToIRArray argTypes st cs) (exprArrToIRArray args st cs) (genIR returnType st cs) (exprArrToIRArray exprs st cs)
    genIR (ModifierBlock exprs) st cs = ModifierBlockIR (IRInfo $ "ModifierBlock") (map (\e -> genIR e st cs) exprs)
    genIR (NewClassInstance className args) st cs = Empty
    genIR (Object packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) st originalState = Empty
    genIR (ObjectMethodCall objectName methodName args) st cs = Empty
    genIR (Print exprs) st cs = Empty
    genIR (Reassign name value) st cs = Empty
    genIR (Return expr) st cs = Empty
    genIR (ReturnType b) st cs = Empty
    genIR (Seq s) st cs = Empty
    genIR (Skip) st cs = Empty
    genIR (StringLiteral value) st cs = Empty
    genIR (Super) st cs = Empty
    genIR (SuperMethodCall objectName methodName args) st cs = Empty
    genIR (This) st cs = Empty
    genIR (ThisMethodCall methodName args) st cs = Empty
    genIR (ThisVar varName) st cs = Empty
    genIR (Trait packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) st originalState = Empty
    genIR (Try exprs) st cs = Empty
    genIR (Type b) st cs = Empty
    genIR (Where exprs) st cs = Empty
    genIR (While condition statement) st cs = Empty

maybeExprToMaybeIRNode :: Maybe Expr -> SymbolTable -> CurrentState -> Maybe IRNode
maybeExprToMaybeIRNode mExpr st cs = case mExpr of
                                                          Just e -> Just $ genIR e st cs
                                                          Nothing -> Nothing

exprArrToIRArray :: [Expr] -> SymbolTable -> CurrentState -> [IRNode]
exprArrToIRArray exprs st cs = (map (\a -> genIR a st cs) exprs)