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
  | Catch (Maybe [Expr]) [Expr]
  | Class [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ClassParam {classParamVarType :: Expr, classParamVarName :: Expr}
  | ClassVariable String String
  | Constructor String [Expr] [Expr] [Expr]
  | Data String [Expr]
  | DataElement String String [String] [String]
  | DataInstance Expr [Expr]
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
  | Lambda String [Expr]
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
    genIR (Annotation name) symbolTable currentState = AnnotationIR (IRInfo $ "Annotation") name
    genIR (Argument a) symbolTable currentState = ArgumentIR (IRInfo $ "Argument") (genIR a symbolTable currentState)
    genIR (ArgumentType aType) symbolTable currentState = ArgumentTypeIR (IRInfo $ "Argument Type") aType
    genIR (ArithExpr aExpr) symbolTable currentState = ArithExprIR (IRInfo $ "Arith Expr") (genIR aExpr symbolTable currentState)
    genIR (ArrayAppend arrays) symbolTable currentState = ArrayAppendIR (IRInfo $ "Array Append") (map (\a -> genIR a symbolTable currentState) arrays)
    genIR (ArrayAssignment arr values) symbolTable currentState = Empty
    genIR (ArrayDef arrType name) symbolTable currentState = Empty
    genIR (ArrayElementSelect i) symbolTable currentState = Empty
    genIR (ArrayType arrType) symbolTable currentState = Empty
    genIR (ArrayValues exprs) symbolTable currentState = Empty
    genIR (Assign vType name value) symbolTable currentState = Empty
    genIR (AssignArith mutable vType name value) symbolTable currentState = Empty
    genIR (BooleanExpr expr) symbolTable currentState = Empty
    genIR (Catch params exprs) symbolTable currentState = Empty
    genIR (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = Empty
    genIR (ClassParam varType varName) symbolTable currentState = Empty
    genIR (ClassVariable className varName) symbolTable currentState = Empty
    genIR (Constructor name argTypes args body) symbolTable currentState = Empty
    genIR (Data name exprs) symbolTable currentState = Empty
    genIR (DataElement superName name argTypes args) symbolTable currentState = Empty
    genIR (Else statement) symbolTable currentState = Empty
    genIR (ElseIf condition statement) symbolTable currentState = Empty
    genIR (For varName start end exprs) symbolTable currentState = Empty
    genIR (Function name annotations argTypes args returnType static body) symbolTable currentState = Empty
    genIR (FunctionCall name exprs) symbolTable currentState = Empty
    genIR (GlobalVar modifier final static varType varName exprs) symbolTable currentState = Empty
    genIR (Identifier name) symbolTable currentState = Empty
    genIR (If condition statement) symbolTable currentState = Empty
    genIR (Import locs) symbolTable currentState = Empty
    genIR (Lambda varName exprs) symbolTable currentState = Empty
    genIR (MainFunction name annotations argTypes args returnType body) symbolTable currentState = Empty
    genIR (ModifierBlock exprs) symbolTable currentState = Empty
    genIR (NewClassInstance className args) symbolTable currentState = Empty
    genIR (Object packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = Empty
    genIR (ObjectMethodCall objectName methodName args) symbolTable currentState = Empty
    genIR (Print exprs) symbolTable currentState = Empty
    genIR (Reassign name value) symbolTable currentState = Empty
    genIR (Return expr) symbolTable currentState = Empty
    genIR (ReturnType b) symbolTable currentState = Empty
    genIR (Seq s) symbolTable currentState = Empty
    genIR (Skip) symbolTable currentState = Empty
    genIR (StringLiteral value) symbolTable currentState = Empty
    genIR (Super) symbolTable currentState = Empty
    genIR (SuperMethodCall objectName methodName args) symbolTable currentState = Empty
    genIR (This) symbolTable currentState = Empty
    genIR (ThisMethodCall methodName args) symbolTable currentState = Empty
    genIR (ThisVar varName) symbolTable currentState = Empty
    genIR (Trait packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState = Empty
    genIR (Try exprs) symbolTable currentState = Empty
    genIR (Type b) symbolTable currentState = Empty
    genIR (Where exprs) symbolTable currentState = Empty
    genIR (While condition statement) symbolTable currentState = Empty
    genIR (_) symbolTable currentState = Empty
