{-|
Module      : Block
Description : Data types that store general expressions.
These are used to store the data and specify how the code is generated.
-}

module Block where

import Data.List
import Text.Show.Functions
import Data.Char


import ABBlock
import SymbolTable




class Debug a where
  debug :: a -> String

class ErrorCheck a where
  errorCheck :: a -> String

class SymbolTableGen a where
  genClassSymbolTable :: a -> ClassSymbolTable

class CodeGen a where
  genCode :: a -> SymbolTable -> CurrentState -> IRNode

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | Parenthesis AExpr
  | AError
  deriving (Eq)

instance CodeGen AExpr where
    genCode (Var v) symbolTable currentState = do
      -- todo
      --let methodList = map (snd) (filter (\x -> fst x == (currentMethodName currentState)) (methods symbolTable))
      --if(length methodList > 0)
      --then if (elem v (map fst (methodArgs (methodList!!0)))) then v else (v ++ "()")
      --else v
      VarIR (IRInfo "var") v
    genCode (IntConst i) symbolTable currentState = IntConstIR (IRInfo "IntConst") i
    genCode (Neg aExpr) symbolTable currentState = "-" ++ genCode aExpr symbolTable currentState
    genCode (ABinary aBinOp aExpr1 aExpr2) symbolTable currentState = genCode aExpr1 symbolTable currentState ++ " " ++ genCode aBinOp symbolTable currentState ++ " " ++ genCode aExpr2 symbolTable currentState
    genCode (Parenthesis aExpr) symbolTable currentState = "(" ++ genCode aExpr symbolTable currentState ++ ")"

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

instance CodeGen ABinOp where
    genCode (Add) symbolTable currentState = "+"
    genCode (Subtract) symbolTable currentState = "-"
    genCode (Multiply) symbolTable currentState = "*"
    genCode (Divide) symbolTable currentState = "/"
    genCode (OpeningParenthesis) currentState symbolTable = "("
    genCode (ClosingParenthesis) currentState symbolTable = ")"

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

instance CodeGen BExpr where
    genCode (BoolConst b) symbolTable currentState = lowerString $ show b
    genCode (Not n) symbolTable currentState = genCode n symbolTable currentState
    genCode (BBinary bbinop bExpr1 bExpr2) symbolTable currentState = genCode bExpr1 symbolTable currentState ++ " " ++ genCode bbinop symbolTable currentState ++ " " ++ genCode bExpr2 symbolTable currentState
    genCode (RBinary rbinop aExpr1 aExpr2) symbolTable currentState = genCode aExpr1 symbolTable currentState ++ " " ++ genCode rbinop symbolTable currentState ++ " " ++ genCode aExpr2 symbolTable currentState

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

instance CodeGen BBinOp where
    genCode (And) symbolTable currentState = "&&"
    genCode (Or) symbolTable currentState = "||"

-- R binary ops
data RBinOp
  = Greater
  | GreaterEqual
  | Less
  | LessEqual
  | RBinOpError
  deriving (Eq)

instance CodeGen RBinOp where
    genCode (Greater) symbolTable currentState = ">"
    genCode (Less) symbolTable currentState = "<"
    genCode (GreaterEqual) symbolTable currentState = ">="
    genCode (LessEqual) symbolTable currentState = "<="

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
  | ClassParam {varType :: Expr, varName :: Expr}
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
  deriving (Eq)

instance ErrorCheck Expr where
  errorCheck (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = "An error occurred"
  errorCheck (_) = "<Unimplemented error check>"

instance SymbolTableGen Expr where
  genClassSymbolTable (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ClassType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
  genClassSymbolTable (ClassParam varType varName) = (ClassSymbolTable "" NoType [(show varName, show varType)] [])
  genClassSymbolTable (Function name annotations argTypes args returnType static body) = ClassSymbolTable "" NoType [] [(name, (MethodSymbolTable (show returnType) (zip (map show args) (map show argTypes))))]
  genClassSymbolTable (GlobalVar modifier final static varType varName exprs) = (ClassSymbolTable "" NoType [(show varName, show varType)] [])
  genClassSymbolTable (ModifierBlock exprs) =  foldl1 (\x y -> combineClassSymbolTable x y) (map genClassSymbolTable exprs)
  genClassSymbolTable (Object packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name ObjectType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
  genClassSymbolTable (Trait packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineClassSymbolTable (combineClassSymbolTable (combineClassSymbolTable (ClassSymbolTable name TraitType [] []) (combineClassSymbolTableList (map genClassSymbolTable params))) (combineClassSymbolTableList (map genClassSymbolTable modifierBlocks))) (combineClassSymbolTableList (map genClassSymbolTable bodyArray))
  genClassSymbolTable (_) = ClassSymbolTable "" NoType [] []

instance Show Expr where
  show (e) = genCode e (SymbolTable [(ClassSymbolTable "" NoType [] [])]) (CurrentState "" "")

instance CodeGen Expr where
    genCode (Annotation name) symbolTable currentState = getDebug "Annotation" ++ "@" ++ name
    genCode (Argument b) symbolTable currentState = getDebug "Argument" ++ genCode b symbolTable currentState
    genCode (ArgumentType b) symbolTable currentState = getDebug "ArgumentType" ++ b
    genCode (ArithExpr aExpr) symbolTable currentState = getDebug "ArithExpr" ++ genCode aExpr symbolTable currentState
    genCode (ArrayAppend arrays) symbolTable currentState = getDebug "ArrayAppend" ++ intercalate "" (map (\arr -> "") arrays)
    genCode (ArrayAssignment arr values) symbolTable currentState = getDebug "ArrayAssignment" ++ genCode arr symbolTable currentState ++ genCode values symbolTable currentState
    genCode (ArrayDef arrType name) symbolTable currentState = getDebug "ArrayDef" ++ arrType ++ "[] " ++ name ++ "="
    genCode (ArrayElementSelect i) symbolTable currentState = getDebug "ArrayElementSelect" ++ "[" ++ i ++ "];"
    genCode (ArrayType arrType) symbolTable currentState = getDebug "ArrayType" ++ arrType ++ "[]"
    genCode (ArrayValues exprs) symbolTable currentState = getDebug "ArrayValues" ++ "{" ++ intercalate ", " exprs ++ "};"
    genCode (Assign vType name value) symbolTable currentState = getDebug "Assign" ++ genCode vType symbolTable currentState ++ " " ++ genCode name symbolTable currentState ++ "=" ++ genCode value symbolTable currentState ++ ";"
    genCode (AssignArith mutable vType name value) symbolTable currentState = getDebug "AssignArith" ++ (if mutable then "" else "final ") ++ genCode vType symbolTable currentState ++ " " ++ name ++ "=" ++ genCode value symbolTable currentState ++ ";"
    genCode (BooleanExpr expr) symbolTable currentState = getDebug "BooleanExpr" ++ genCode expr symbolTable currentState
    genCode (Catch params exprs) symbolTable currentState = getDebug "Catch" ++ "catch(" ++  intercalate ", " (map (\x -> "final " ++ genCode x symbolTable currentState) paramList)  ++ "){" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs) ++ "}"
      where
        paramList = case params of
            Just a -> a
            Nothing -> []

    genCode (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        getDebug "Class" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "") ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) imports) ++
        "public class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) modifierBlocks) ++
        intercalate " " (map (\x -> "private " ++ genCode x symbolTable currentState ++ ";") params) ++

       -- Create getter methods for constructor params
        intercalate " " (map (\x -> "public " ++ genCode (varType x) symbolTable currentState ++ " " ++ genCode (varName x) symbolTable currentState ++ "(){return " ++ genCode (varName x) symbolTable currentState ++ ";}") params) ++

       -- Create setter methods for constructor params
        intercalate " " (map (\x -> "public void " ++ genCode (varName x) symbolTable currentState ++ "_(" ++ genCode (varType x) symbolTable currentState ++ " " ++ genCode (varName x) symbolTable currentState ++ "){ this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";}") params) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> genCode x symbolTable currentState) params) ++"){" ++
        intercalate " " (map (\x -> "this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";") params) ++
        intercalate " " (map (\x -> genCode x symbolTable currentState) constructorExprs) ++ "}" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          currentState = CurrentState name ""
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              [] -> ""
              ws -> "implements " ++ (intercalate "," ws)

    genCode (ClassParam varType varName) symbolTable currentState = getDebug "ClassParam" ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState
    genCode (ClassVariable className varName) symbolTable currentState = getDebug "ClassVariable" ++
      case (instanceVariableType symbolTable (currentClassName currentState) className) of
        Just varType -> do
          if(instanceVariableExists symbolTable varType varName)
            then className ++ "." ++ varName ++ "()"
            else if(instanceVariableExists symbolTable (currentClassName currentState) varName)
              then className ++ "." ++ varName ++ "()"
              else if(instanceVariableExists symbolTable className varName)
                then className ++ ".getInstance()." ++ varName ++ "()"
                else className ++ "." ++ varName
        Nothing -> do
          if(instanceVariableExists symbolTable (currentClassName currentState) varName)
            then  className ++ "." ++ varName ++ "()"
            else if(instanceVariableExists symbolTable className varName)
              then className ++ ".getInstance()." ++ varName ++ "()"
              else className ++ "." ++ varName

    genCode (Constructor name argTypes args body) symbolTable currentState = getDebug "Constructor" ++ "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable currentState) argTypes) (map (\x -> genCode x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) body) ++ "}"
    genCode (Data name exprs) symbolTable currentState = getDebug "Data" ++ "class " ++ name ++ "{}" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (DataElement superName name argTypes args) symbolTable currentState = getDebug "DataElement" ++ "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"

    genCode (Else statement) symbolTable currentState = getDebug "Else" ++ " else {\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (ElseIf condition statement) symbolTable currentState = getDebug "ElseIf" ++ " else if(" ++ genCode condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (For varName start end exprs) symbolTable currentState = getDebug "For" ++ "for(" ++ "int " ++ varName ++ "=" ++ show start ++ ";" ++ varName ++ "<" ++ show end ++ ";" ++ varName ++ "++){" ++ intercalate " " (map show exprs) ++ "}"
    genCode (Function name annotations argTypes args returnType static body) symbolTable currentState =
      getDebug "Function" ++
      annotationString ++ " public " ++ (if(static) then "static " else "") ++ genCode returnType symbolTable (CurrentState (currentClassName currentState) name) ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable (CurrentState (currentClassName currentState) name)) argTypes) (map (\x -> genCode x symbolTable (CurrentState (currentClassName currentState) name)) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable (CurrentState (currentClassName currentState) name)) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> genCode a symbolTable currentState
              Nothing -> ""

    genCode (FunctionCall name exprs) symbolTable currentState = getDebug "FunctionCall" ++ name ++ "(" ++ (intercalate ", " (map (\x -> genCode x symbolTable currentState) exprs)) ++ ");"
    genCode (GlobalVar modifier final static varType varName exprs) symbolTable currentState =
      getDebug "GlobalVar" ++
      "private " ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState ++ ";" ++ -- ++ "=" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs) ++ ";" ++
      -- Bool to store if the value has been set yet
      "private boolean " ++ genCode varName symbolTable currentState ++ "Bool=false;" ++
      -- Create getter method
      modifier ++ " " ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState ++ "(){ if(!" ++ genCode varName symbolTable currentState ++ "Bool){" ++ genCode varName symbolTable currentState ++ "Bool=true;" ++ genCode varName symbolTable currentState ++ "=" ++ intercalate " " (map (\e -> genCode e symbolTable currentState ++ ";") exprs)  ++ "}return " ++ genCode varName symbolTable currentState ++ ";}" ++
      -- If it isn't final create a setter method
      if(not final)
        then modifier ++ " void " ++ genCode varName symbolTable currentState ++ "_(final " ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState ++ "){this." ++ genCode varName symbolTable currentState ++ "Bool=true;" ++ "this." ++ genCode varName symbolTable currentState ++ "=" ++ genCode varName symbolTable currentState ++ ";}"
        else ""

    genCode (Identifier name) symbolTable currentState = do
      case (instanceVariableType symbolTable (currentClassName currentState) name) of
        Just varType -> do
          if instanceVariableExists symbolTable varType name
            then name ++ "()"
            else name
        Nothing -> name
    genCode (If condition statement) symbolTable currentState = getDebug "If" ++ "if(" ++ genCode condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (Import locs) symbolTable currentState = getDebug "Import" ++ "import " ++ intercalate "." locs ++ ";"
    genCode (Lambda varName exprs) symbolTable currentState = getDebug "Lambda" ++ "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (MainFunction name annotations argTypes args returnType body) symbolTable currentState =
      getDebug "MainFunction" ++
      annotationString ++ "public static " ++ genCode returnType symbolTable currentState ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable currentState) argTypes) (map (\x -> genCode x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> genCode a symbolTable currentState
              Nothing -> ""
    genCode (ModifierBlock exprs) symbolTable currentState = getDebug "ModifierBlock" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (NewClassInstance className args) symbolTable currentState = getDebug "NewClassInstance" ++ "new " ++ className ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ")"
    genCode (Object packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        getDebug "Module" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++

        "private final static " ++ name ++ " instance = new " ++ name ++ "();" ++
        "public final static " ++ name ++ " getInstance(){return instance;}" ++

        intercalate "\n" (map (\x -> genCode x symbolTable currentState) modifierBlocks) ++

        intercalate " " (map (\x -> "private final " ++ genCode x symbolTable currentState ++ ";") params) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> genCode x symbolTable currentState) params) ++"){" ++

        intercalate " " (map (\x -> "this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";") params) ++
        intercalate " " (map (\x -> genCode x symbolTable currentState) constructorExprs) ++ "}" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          currentState = CurrentState name ""
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              [] -> ""
              ws -> "implements " ++ (intercalate "," ws)

    genCode (ObjectMethodCall objectName methodName args) symbolTable currentState = objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (Print exprs) symbolTable currentState = getDebug "Print" ++ "System.out.println(" ++ genCode exprs symbolTable currentState ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    genCode (Reassign name value) symbolTable currentState = getDebug "Reassign" ++ show name ++ "_(" ++ genCode value symbolTable currentState++ ");"
    genCode (Return expr) symbolTable currentState = getDebug "Return" ++ "return " ++ genCode expr symbolTable currentState ++ ";"
    genCode (ReturnType b) symbolTable currentState = getDebug "ReturnType" ++ b
    genCode (Seq s) symbolTable currentState = getDebug "Seq" ++ "[seq]"
    genCode (Skip) symbolTable currentState = getDebug "Skip" ++ "[skip]"
    genCode (StringLiteral value) symbolTable currentState = getDebug "StringLiteral" ++ "\"" ++ value ++ "\""
    genCode (Super) symbolTable currentState = getDebug "Super" ++ "super"
    genCode (SuperMethodCall objectName methodName args) symbolTable currentState = getDebug "SuperMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (This) symbolTable currentState = getDebug "This" ++ "this"
    genCode (ThisMethodCall methodName args) symbolTable currentState = getDebug "ThisMethodCall" ++methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (ThisVar varName) symbolTable currentState = getDebug "ThisVar" ++ "this." ++ show varName
    genCode (Trait packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        getDebug "interface " ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) imports) ++
        "public class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) modifierBlocks) ++

        intercalate " " (map (\x -> "private " ++ genCode x symbolTable currentState ++ ";") params) ++

       -- Create getter methods for constructor params
        intercalate " " (map (\x -> "public " ++ genCode (varType x) symbolTable currentState ++ " " ++ genCode (varName x) symbolTable currentState ++ "(){return " ++ genCode (varName x) symbolTable currentState ++ ";}") params) ++

       -- Create setter methods for constructor params
        intercalate " " (map (\x -> "public void " ++ genCode (varName x) symbolTable currentState ++ "_(" ++ genCode (varType x) symbolTable currentState ++ " " ++ genCode (varName x) symbolTable currentState ++ "){ this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";}") params) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> genCode x symbolTable currentState) params) ++"){" ++

        intercalate " " (map (\x -> "this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";") params) ++
        intercalate " " (map (\x -> genCode x symbolTable currentState) constructorExprs) ++ "}" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          currentState = CurrentState name ""
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              [] -> ""
              ws -> "implements " ++ (intercalate "," ws)

    genCode (Try exprs) symbolTable currentState = getDebug "Try" ++ "try{" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs) ++ "}"
    genCode (Type b) symbolTable currentState = getDebug "Type" ++ genCode b symbolTable currentState
    genCode (Where exprs) symbolTable currentState = getDebug "Where" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (While condition statement) symbolTable currentState = getDebug "While" ++ "while(" ++ genCode condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (_) symbolTable currentState = "<unknown>"
