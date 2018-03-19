module IRNode where

import Data.List
import Data.Maybe
import Data.Char

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


data IRNode
  = AnnotationIR IRInfo String
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
  | BooleanExprIR IRInfo IRNode
  | CatchIR IRInfo (Maybe [IRNode]) [IRNode]
  | ClassIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ClassParamIR {classPIRInfo :: IRInfo, varType :: IRNode, varName :: IRNode}
  | ClassVariableIR IRInfo String String
  | ConstructorIR IRInfo String [IRNode] [IRNode] [IRNode]
  | DataIR IRInfo String [IRNode]
  | DataElementIR IRInfo String String [String] [String]
  | DataInstanceIR IRInfo IRNode [IRNode]
  | ElseIR IRInfo [IRNode]
  | ElseIfIR IRInfo IRNode [IRNode]
  | ErrorIR IRInfo
  | ForIR IRInfo String IRNode IRNode [IRNode]
  | FunctionIR IRInfo String (Maybe IRNode) [IRNode] [IRNode] IRNode Bool [IRNode]
  | FunctionCallIR IRInfo String [IRNode]
  | GlobalVarIR IRInfo String Bool Bool IRNode IRNode [IRNode]
  | IdentifierIR IRInfo String
  | IfIR IRInfo IRNode [IRNode]
  | ImportIR {importIRInfo:: IRInfo, locs ::[String]}
  | LambdaIR IRInfo String [IRNode]
  | MainFunctionIR {mainFunctionIRInfoVal :: IRInfo, name ::String, annotations :: (Maybe IRNode), argTypes:: [IRNode], args::[IRNode], returnType::IRNode, body::[IRNode]}
  | ModifierBlockIR IRInfo [IRNode]
  | NewClassInstanceIR IRInfo String [IRNode]
  | ObjectIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ObjectMethodCallIR IRInfo String String [IRNode]
  | PrintIR IRInfo IRNode
  | ReassignIR IRInfo IRNode IRNode
  | ReturnIR IRInfo IRNode
  | ReturnTypeIR IRInfo String
  | SeqIR IRInfo [IRNode]
  | SkipIR IRInfo
  | StringLiteralIR IRInfo String
  | SuperIR IRInfo
  | SuperMethodCallIR IRInfo String String [IRNode]
  | ThisIR IRInfo
  | ThisMethodCallIR IRInfo String [IRNode]
  | ThisVarIR IRInfo IRNode
  | TraitIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | TryIR IRInfo [IRNode]
  | TypeIR IRInfo IRNode
  | WhereIR IRInfo [IRNode]
  | WhileIR IRInfo IRNode [IRNode]
  deriving (Eq)

data IRInfo = IRInfo String
  deriving (Eq)


instance Pretty IRNode where
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
    pretty (ForIR irInfo varName start end exprs) symbolTable currentState = getDebug "For" ++ "for(" ++ "int " ++ varName ++ "=" ++ pretty start symbolTable currentState ++ ";" ++ varName ++ "<" ++ pretty end symbolTable currentState ++ ";" ++ varName ++ "++){" ++ intercalate " " (map (\e -> pretty e symbolTable currentState) exprs) ++ "}"
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
    pretty (ReassignIR irInfo name value) symbolTable currentState = getDebug "Reassign" ++ pretty name symbolTable currentState ++ "_(" ++ pretty value symbolTable currentState++ ");"
    pretty (ReturnIR irInfo expr) symbolTable currentState = getDebug "Return" ++ "return " ++ pretty expr symbolTable currentState ++ ";"
    pretty (ReturnTypeIR irInfo b) symbolTable currentState = getDebug "ReturnType" ++ b
    pretty (SeqIR irInfo s) symbolTable currentState = getDebug "Seq" ++ "[seq]"
    pretty (SkipIR irInfo) symbolTable currentState = getDebug "Skip" ++ "[skip]"
    pretty (StringLiteralIR irInfo value) symbolTable currentState = getDebug "StringLiteral" ++ "\"" ++ value ++ "\""
    pretty (SuperIR irInfo) symbolTable currentState = getDebug "Super" ++ "super"
    pretty (SuperMethodCallIR irInfo objectName methodName args) symbolTable currentState = getDebug "SuperMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (ThisIR irInfo) symbolTable currentState = getDebug "This" ++ "this"
    pretty (ThisMethodCallIR irInfo methodName args) symbolTable currentState = getDebug "ThisMethodCall" ++methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (ThisVarIR irInfo varName) symbolTable currentState = getDebug "ThisVar" ++ "this." ++ pretty varName symbolTable currentState
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


lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: IRNode -> Maybe [String]
extractImportStatement (ImportIR _ m) = Just m
extractImportStatement _ = Nothing

isImportStatement :: IRNode -> Bool
isImportStatement e = isJust $ extractImportStatement e
