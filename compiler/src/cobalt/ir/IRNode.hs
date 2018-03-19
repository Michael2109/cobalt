module IRNode where

import Data.List

import SymbolTable

{--import Text.PrettyPrint.ANSI.Leijen
       ( Doc
       , int, text, char
       , (</>), (<//>), (<+>), (<>)
       , parens
       )--}

import ABBlock
import Block


class Pretty a where
  pretty :: a -> SymbolTable -> CurrentState -> String -- Pretty document type


data IRTree
  = SeqIR IRInfo [Expr]
  | ImportIR IRInfo [String]
  | GlobalVarIR IRInfo  String Bool Bool Expr Expr [Expr]
  | MainFunctionIR IRInfo String (Maybe Expr) [Expr] [Expr] Expr [Expr]
  | FunctionIR IRInfo  String (Maybe Expr) [Expr] [Expr] Expr Bool [Expr]
  | ConstructorIR IRInfo  String [Expr] [Expr] [Expr]
  | FunctionCallIR IRInfo  String [Expr]
  | TypeIR IRInfo  Expr
  | ArgumentIR IRInfo  Expr
  | ArgumentTypeIR IRInfo  String
  | ReturnTypeIR IRInfo  String
  | AssignArithIR IRInfo  Bool Expr String Expr
  | ArithExprIR IRInfo  AExpr
  | ArrayTypeIR IRInfo  String
  | ArrayAppendIR IRInfo  [Expr]
  | AssignIR IRInfo  Expr Expr Expr
  | ReassignIR IRInfo  Expr Expr
  | IfIR IRInfo  Expr [Expr]
  | ElseIfIR IRInfo  Expr [Expr]
  | ElseIR IRInfo  [Expr]
  | TryIR IRInfo  [Expr]
  | CatchIR IRInfo  (Maybe [Expr]) [Expr]
  | WhileIR IRInfo  Expr [Expr]
  | ForIR IRInfo  String Expr Expr [Expr]
  | PrintIR IRInfo  Expr
  | ReturnIR IRInfo  Expr
  | ArrayValuesIR IRInfo  [String]
  | ArrayDefIR IRInfo  String String
  | ArrayAssignmentIR IRInfo  Expr Expr
  | ArrayElementSelectIR IRInfo  String
  | WhereIR IRInfo  [Expr]
  | StringLiteralIR IRInfo  String
  | DataIR IRInfo  String [Expr]
  | DataElementIR IRInfo  String String [String] [String]
  | DataInstanceIR IRInfo  Expr [Expr]
  | SuperMethodCallIR IRInfo  String String [Expr]
  | ObjectMethodCallIR IRInfo  String String [Expr]
  | ThisVarIR IRInfo  Expr
  | ThisMethodCallIR IRInfo  String [Expr]
  | NewClassInstanceIR IRInfo  String [Expr]
  | ClassVariableIR IRInfo  String String
  | BooleanExprIR IRInfo  BExpr
  | IdentifierIR IRInfo  String
  | AnnotationIR IRInfo  String
  | ModifierBlockIR IRInfo  [Expr]
  | ThisIR IRInfo
  | SuperIR IRInfo
  | LambdaIR IRInfo  String [Expr]
  | ClassParamIR IRInfo Expr Expr
  | SkipIR IRInfo
  | ErrorIR IRInfo
  | ObjectIR IRInfo [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | ClassIR IRInfo  [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  | TraitIR IRInfo  [String] String [Expr] (Maybe String) [String] [Expr] [Expr] [Expr] [Expr]
  deriving (Eq)

data IRInfo = IRInfo String
  deriving (Eq)


instance Pretty IRTree where
    pretty (AnnotationIR irInfo name) symbolTable currentState = getDebug "Annotation" ++ "@" ++ name
    pretty (ArgumentIR irInfo b) symbolTable currentState = getDebug "Argument" ++ pretty b symbolTable currentState
    pretty (ArgumentTypeIR irInfo b) symbolTable currentState = getDebug "ArgumentType" ++ b
    pretty (ArithExprIR irInfo aExpr) symbolTable currentState = getDebug "ArithExpr" ++ pretty aExpr symbolTable currentState
    pretty (ArrayAppendIR irInfo arrays) symbolTable currentState = getDebug "ArrayAppend" ++ intercalate "" (map (\arr -> "") arrays)
    pretty (ArrayAssignmentIR irInfo arr values) symbolTable currentState = getDebug "ArrayAssignment" ++ pretty arr symbolTable currentState ++ pretty values symbolTable currentState
    pretty (ArrayDefIR irInfo arrType name) symbolTable currentState = getDebug "ArrayDef" ++ arrType ++ "[] " ++ name ++ "="
    pretty (ArrayElementSelectIR irInfo i) symbolTable currentState = getDebug "ArrayElementSelect" ++ "[" ++ i ++ "];"
    pretty (ArrayTypeIR irInfo arrType) symbolTable currentState = getDebug "ArrayType" ++ arrType ++ "[]"
    pretty (ArrayValuesIR irInfo exprs) symbolTable currentState = getDebug "ArrayValues" ++ "{" ++ intercalate ", " exprs ++ "};"
    pretty (AssignIR irInfo vType name value) symbolTable currentState = getDebug "Assign" ++ pretty vType symbolTable currentState ++ " " ++ pretty name symbolTable currentState ++ "=" ++ pretty value symbolTable currentState ++ ";"
    pretty (AssignArithIR irInfo mutable vType name value) symbolTable currentState = getDebug "AssignArith" ++ (if mutable then "" else "final ") ++ pretty vType symbolTable currentState ++ " " ++ name ++ "=" ++ pretty value symbolTable currentState ++ ";"
    pretty (BooleanExprIR irInfo expr) symbolTable currentState = getDebug "BooleanExpr" ++ pretty expr symbolTable currentState
    pretty (CatchIR irInfo params exprs) symbolTable currentState = getDebug "Catch" ++ "catch(" ++  intercalate ", " (map (\x -> "final " ++ pretty x symbolTable currentState) paramList)  ++ "){" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs) ++ "}"
      where
        paramList = case params of
            Just a -> a
            Nothing -> []

    pretty (ClassIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        getDebug "Class" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "") ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) imports) ++
        "public class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) modifierBlocks) ++
        intercalate " " (map (\x -> "private " ++ pretty x symbolTable currentState ++ ";") params) ++

       -- Create getter methods for constructor params
        intercalate " " (map (\x -> "public " ++ pretty (varType x) symbolTable currentState ++ " " ++ pretty (varName x) symbolTable currentState ++ "(){return " ++ pretty (varName x) symbolTable currentState ++ ";}") params) ++

       -- Create setter methods for constructor params
        intercalate " " (map (\x -> "public void " ++ pretty (varName x) symbolTable currentState ++ "_(" ++ pretty (varType x) symbolTable currentState ++ " " ++ pretty (varName x) symbolTable currentState ++ "){ this." ++ pretty (varName x) symbolTable currentState ++ "=" ++ pretty (varName x) symbolTable currentState ++ ";}") params) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> pretty x symbolTable currentState) params) ++"){" ++
        intercalate " " (map (\x -> "this." ++ pretty (varName x) symbolTable currentState ++ "=" ++ pretty (varName x) symbolTable currentState ++ ";") params) ++
        intercalate " " (map (\x -> pretty x symbolTable currentState) constructorExprs) ++ "}" ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          currentState = CurrentState name ""
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              [] -> ""
              ws -> "implements " ++ (intercalate "," ws)

    pretty (ClassParamIR irInfo varType varName) symbolTable currentState = getDebug "ClassParam" ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState
    pretty (ClassVariableIR irInfo className varName) symbolTable currentState = getDebug "ClassVariable" ++
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

    pretty (ConstructorIR irInfo name argTypes args body) symbolTable currentState = getDebug "Constructor" ++ "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> pretty x symbolTable currentState) argTypes) (map (\x -> pretty x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) body) ++ "}"
    pretty (DataIR irInfo name exprs) symbolTable currentState = getDebug "Data" ++ "class " ++ name ++ "{}" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (DataElementIR irInfo superName name argTypes args) symbolTable currentState = getDebug "DataElement" ++ "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"

    pretty (ElseIR irInfo statement) symbolTable currentState = getDebug "Else" ++ " else {\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (ElseIfIR irInfo condition statement) symbolTable currentState = getDebug "ElseIf" ++ " else if(" ++ pretty condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (ForIR irInfo varName start end exprs) symbolTable currentState = getDebug "For" ++ "for(" ++ "int " ++ varName ++ "=" ++ show start ++ ";" ++ varName ++ "<" ++ show end ++ ";" ++ varName ++ "++){" ++ intercalate " " (map show exprs) ++ "}"
    pretty (FunctionIR irInfo name annotations argTypes args returnType static body) symbolTable currentState =
      getDebug "Function" ++
      annotationString ++ " public " ++ (if(static) then "static " else "") ++ pretty returnType symbolTable (CurrentState (currentClassName currentState) name) ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> pretty x symbolTable (CurrentState (currentClassName currentState) name)) argTypes) (map (\x -> pretty x symbolTable (CurrentState (currentClassName currentState) name)) args)) ++"){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable (CurrentState (currentClassName currentState) name)) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> pretty a symbolTable currentState
              Nothing -> ""

    pretty (FunctionCallIR irInfo name exprs) symbolTable currentState = getDebug "FunctionCall" ++ name ++ "(" ++ (intercalate ", " (map (\x -> pretty x symbolTable currentState) exprs)) ++ ");"
    pretty (GlobalVarIR irInfo modifier final static varType varName exprs) symbolTable currentState =
      getDebug "GlobalVar" ++
      "private " ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState ++ ";" ++ -- ++ "=" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs) ++ ";" ++
      -- Bool to store if the value has been set yet
      "private boolean " ++ pretty varName symbolTable currentState ++ "Bool=false;" ++
      -- Create getter method
      modifier ++ " " ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState ++ "(){ if(!" ++ pretty varName symbolTable currentState ++ "Bool){" ++ pretty varName symbolTable currentState ++ "Bool=true;" ++ pretty varName symbolTable currentState ++ "=" ++ intercalate " " (map (\e -> pretty e symbolTable currentState ++ ";") exprs)  ++ "}return " ++ pretty varName symbolTable currentState ++ ";}" ++
      -- If it isn't final create a setter method
      if(not final)
        then modifier ++ " void " ++ pretty varName symbolTable currentState ++ "_(final " ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState ++ "){this." ++ pretty varName symbolTable currentState ++ "Bool=true;" ++ "this." ++ pretty varName symbolTable currentState ++ "=" ++ pretty varName symbolTable currentState ++ ";}"
        else ""

    pretty (IdentifierIR irInfo name) symbolTable currentState = do
      case (instanceVariableType symbolTable (currentClassName currentState) name) of
        Just varType -> do
          if instanceVariableExists symbolTable varType name
            then name ++ "()"
            else name
        Nothing -> name
    pretty (IfIR irInfo condition statement) symbolTable currentState = getDebug "If" ++ "if(" ++ pretty condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (ImportIR irInfo locs) symbolTable currentState = getDebug "Import" ++ "import " ++ intercalate "." locs ++ ";"
    pretty (LambdaIR irInfo varName exprs) symbolTable currentState = getDebug "Lambda" ++ "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (MainFunctionIR irInfo name annotations argTypes args returnType body) symbolTable currentState =
      getDebug "MainFunction" ++
      annotationString ++ "public static " ++ pretty returnType symbolTable currentState ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> pretty x symbolTable currentState) argTypes) (map (\x -> pretty x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> pretty a symbolTable currentState
              Nothing -> ""
    pretty (ModifierBlockIR irInfo exprs) symbolTable currentState = getDebug "ModifierBlock" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (NewClassInstanceIR irInfo className args) symbolTable currentState = getDebug "NewClassInstance" ++ "new " ++ className ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ")"
    pretty (ObjectIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        getDebug "Module" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++

        "private final static " ++ name ++ " instance = new " ++ name ++ "();" ++
        "public final static " ++ name ++ " getInstance(){return instance;}" ++

        intercalate "\n" (map (\x -> pretty x symbolTable currentState) modifierBlocks) ++

        intercalate " " (map (\x -> "private final " ++ pretty x symbolTable currentState ++ ";") params) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> pretty x symbolTable currentState) params) ++"){" ++

        intercalate " " (map (\x -> "this." ++ pretty (varName x) symbolTable currentState ++ "=" ++ pretty (varName x) symbolTable currentState ++ ";") params) ++
        intercalate " " (map (\x -> pretty x symbolTable currentState) constructorExprs) ++ "}" ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          currentState = CurrentState name ""
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              [] -> ""
              ws -> "implements " ++ (intercalate "," ws)

    pretty (ObjectMethodCallIR irInfo objectName methodName args) symbolTable currentState = objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (PrintIR irInfo exprs) symbolTable currentState = getDebug "Print" ++ "System.out.println(" ++ pretty exprs symbolTable currentState ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    pretty (ReassignIR irInfo name value) symbolTable currentState = getDebug "Reassign" ++ show name ++ "_(" ++ pretty value symbolTable currentState++ ");"
    pretty (ReturnIR irInfo expr) symbolTable currentState = getDebug "Return" ++ "return " ++ pretty expr symbolTable currentState ++ ";"
    pretty (ReturnTypeIR irInfo b) symbolTable currentState = getDebug "ReturnType" ++ b
    pretty (SeqIR irInfo s) symbolTable currentState = getDebug "Seq" ++ "[seq]"
    pretty (SkipIR irInfo) symbolTable currentState = getDebug "Skip" ++ "[skip]"
    pretty (StringLiteralIR irInfo value) symbolTable currentState = getDebug "StringLiteral" ++ "\"" ++ value ++ "\""
    pretty (SuperIR irInfo) symbolTable currentState = getDebug "Super" ++ "super"
    pretty (SuperMethodCallIR irInfo objectName methodName args) symbolTable currentState = getDebug "SuperMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (ThisIR irInfo) symbolTable currentState = getDebug "This" ++ "this"
    pretty (ThisMethodCallIR irInfo methodName args) symbolTable currentState = getDebug "ThisMethodCall" ++methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (ThisVarIR irInfo varName) symbolTable currentState = getDebug "ThisVar" ++ "this." ++ show varName
    pretty (TraitIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        getDebug "interface " ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) imports) ++
        "public class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) modifierBlocks) ++

        intercalate " " (map (\x -> "private " ++ pretty x symbolTable currentState ++ ";") params) ++

       -- Create getter methods for constructor params
        intercalate " " (map (\x -> "public " ++ pretty (varType x) symbolTable currentState ++ " " ++ pretty (varName x) symbolTable currentState ++ "(){return " ++ pretty (varName x) symbolTable currentState ++ ";}") params) ++

       -- Create setter methods for constructor params
        intercalate " " (map (\x -> "public void " ++ pretty (varName x) symbolTable currentState ++ "_(" ++ pretty (varType x) symbolTable currentState ++ " " ++ pretty (varName x) symbolTable currentState ++ "){ this." ++ pretty (varName x) symbolTable currentState ++ "=" ++ pretty (varName x) symbolTable currentState ++ ";}") params) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> pretty x symbolTable currentState) params) ++"){" ++

        intercalate " " (map (\x -> "this." ++ pretty (varName x) symbolTable currentState ++ "=" ++ pretty (varName x) symbolTable currentState ++ ";") params) ++
        intercalate " " (map (\x -> pretty x symbolTable currentState) constructorExprs) ++ "}" ++
        intercalate "\n" (map (\x -> pretty x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          currentState = CurrentState name ""
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              [] -> ""
              ws -> "implements " ++ (intercalate "," ws)

    pretty (TryIR irInfo exprs) symbolTable currentState = getDebug "Try" ++ "try{" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs) ++ "}"
    pretty (TypeIR irInfo b) symbolTable currentState = getDebug "Type" ++ pretty b symbolTable currentState
    pretty (WhereIR irInfo exprs) symbolTable currentState = getDebug "Where" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (WhileIR irInfo condition statement) symbolTable currentState = getDebug "While" ++ "while(" ++ pretty condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (_) symbolTable currentState = "<unknown>"