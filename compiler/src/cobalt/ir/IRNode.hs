module IRNode where

import Data.List
import Data.Maybe
import Data.Char
import Text.Format

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

data IRInfo = IRInfo String
  deriving (Eq)

instance Show IRInfo where
  show (IRInfo info) = if debug then format "/*{0}*/" [info] else ""


data IRNode
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
  | ArrayAssignmentIR IRInfo IRNode IRNode
  | ArrayDefIR IRInfo String String
  | ArrayElementSelectIR IRInfo String
  | ArrayTypeIR IRInfo String
  | ArrayValuesIR IRInfo [String]
  | AssignIR IRInfo IRNode IRNode IRNode
  | AssignArithIR IRInfo Bool IRNode String IRNode
  | BBinaryIR IRInfo IRNode IRNode IRNode
  | BBinOpErrorIR IRInfo
  | BErrorIR IRInfo
  | BoolConstIR IRInfo Bool
  | BooleanExprIR IRInfo IRNode
  | CatchIR IRInfo [IRNode] [IRNode]
  | ClassIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ClassParamIR {classPIRInfo :: IRInfo, varType :: IRNode, varName :: IRNode}
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
  | FunctionIR IRInfo String (Maybe IRNode) [IRNode] [IRNode] IRNode Bool [IRNode]
  | FunctionCallIR IRInfo String [IRNode]
  | GlobalVarIR IRInfo String Bool Bool IRNode IRNode [IRNode]
  | GreaterEqualIR IRInfo
  | GreaterIR IRInfo
  | IdentifierIR IRInfo String
  | IfIR IRInfo IRNode [IRNode]
  | ImportIR {importIRInfo:: IRInfo, locs ::[String]}
  | LambdaIR IRInfo String [IRNode]
  | LessIR IRInfo
  | LessEqualIR IRInfo
  | IntConstIR IRInfo Integer
  | MainFunctionIR {mainFunctionIRInfoVal :: IRInfo, name ::String, annotations :: (Maybe IRNode), argTypes:: [IRNode], args::[IRNode], returnType::IRNode, body::[IRNode]}
  | ModifierBlockIR IRInfo [IRNode]
  | MultiplyIR IRInfo
  | NegIR IRInfo IRNode
  | NewClassInstanceIR IRInfo String [IRNode]
  | NotIR IRInfo IRNode
  | ObjectIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | ObjectMethodCallIR IRInfo String String [IRNode]
  | OpeningParenthesisIR IRInfo
  | OrIR IRInfo
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
  | SuperMethodCallIR IRInfo String String [IRNode]
  | ThisIR IRInfo
  | ThisMethodCallIR IRInfo String [IRNode]
  | ThisVarIR IRInfo IRNode
  | TraitIR IRInfo [String] String [IRNode] (Maybe String) [String] [IRNode] [IRNode] [IRNode] [IRNode]
  | TryIR IRInfo [IRNode]
  | TypeIR IRInfo IRNode
  | VarIR IRInfo String
  | WhereIR IRInfo [IRNode]
  | WhileIR IRInfo IRNode [IRNode]
  deriving (Eq, Show)


instance Pretty IRNode where
    pretty (AnnotationIR irInfo name) symbolTable currentState = show irInfo ++ "@" ++ name
    pretty (ArgumentIR irInfo b) symbolTable currentState = show irInfo ++ pretty b symbolTable currentState
    pretty (ArgumentTypeIR irInfo b) symbolTable currentState = show irInfo ++ b
    pretty (ArithExprIR irInfo aExpr) symbolTable currentState = show irInfo ++ pretty aExpr symbolTable currentState
    pretty (ArrayAppendIR irInfo arrays) symbolTable currentState = show irInfo ++ intercalate "" (map (\arr -> "") arrays)
    pretty (ArrayAssignmentIR irInfo arr values) symbolTable currentState = show irInfo ++ pretty arr symbolTable currentState ++ pretty values symbolTable currentState
    pretty (ArrayDefIR irInfo arrType name) symbolTable currentState = show irInfo ++ arrType ++ "[] " ++ name ++ "="
    pretty (ArrayElementSelectIR irInfo i) symbolTable currentState = show  irInfo ++ "[" ++ i ++ "];"
    pretty (ArrayTypeIR irInfo arrType) symbolTable currentState = show irInfo ++ arrType ++ "[]"
    pretty (ArrayValuesIR irInfo exprs) symbolTable currentState = show irInfo ++ "{" ++ intercalate ", " exprs ++ "};"
    pretty (AssignIR irInfo vType name value) symbolTable currentState = show irInfo ++ pretty vType symbolTable currentState ++ " " ++ pretty name symbolTable currentState ++ "=" ++ pretty value symbolTable currentState ++ ";"
    pretty (AssignArithIR irInfo mutable vType name value) symbolTable currentState = show irInfo ++ (if mutable then "" else "final ") ++ pretty vType symbolTable currentState ++ " " ++ name ++ "=" ++ pretty value symbolTable currentState ++ ";"
    pretty (BooleanExprIR irInfo expr) symbolTable currentState = show irInfo ++ pretty expr symbolTable currentState
    pretty (CatchIR irInfo params exprs) symbolTable currentState = show irInfo ++ "catch(" ++  intercalate ", " (map (\x -> "final " ++ pretty x symbolTable currentState) params)  ++ "){" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs) ++ "}"
    pretty (ClassIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        show irInfo ++
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

    pretty (ClassParamIR irInfo varType varName) symbolTable currentState = show irInfo ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState
    pretty (ClassVariableIR irInfo className varName) symbolTable currentState = show irInfo ++
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

    pretty (ConstructorIR irInfo name argTypes args body) symbolTable currentState = show irInfo ++ "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> pretty x symbolTable currentState) argTypes) (map (\x -> pretty x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) body) ++ "}"
    pretty (DataIR irInfo name exprs) symbolTable currentState = show irInfo ++ "class " ++ name ++ "{}" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (DataElementIR irInfo superName name argTypes args) symbolTable currentState = show irInfo ++ "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"

    pretty (ElseIR irInfo statement) symbolTable currentState = show irInfo ++ " else {\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (ElseIfIR irInfo condition statement) symbolTable currentState = show irInfo ++ " else if(" ++ pretty condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (ForIR irInfo varName start end exprs) symbolTable currentState = show irInfo ++ "for(" ++ "int " ++ varName ++ "=" ++ pretty start symbolTable currentState ++ ";" ++ varName ++ "<" ++ pretty end symbolTable currentState ++ ";" ++ varName ++ "++){" ++ intercalate " " (map (\e -> pretty e symbolTable currentState) exprs) ++ "}"
    pretty (FunctionIR irInfo name annotations argTypes args returnType static body) symbolTable currentState =
      show irInfo ++
      annotationString ++ " public " ++ (if(static) then "static " else "") ++ pretty returnType symbolTable (CurrentState (currentClassName currentState) name) ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> pretty x symbolTable (CurrentState (currentClassName currentState) name)) argTypes) (map (\x -> pretty x symbolTable (CurrentState (currentClassName currentState) name)) args)) ++"){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable (CurrentState (currentClassName currentState) name)) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> pretty a symbolTable currentState
              Nothing -> ""

    pretty (FunctionCallIR irInfo name exprs) symbolTable currentState = show irInfo ++ name ++ "(" ++ (intercalate ", " (map (\x -> pretty x symbolTable currentState) exprs)) ++ ");"
    pretty (GlobalVarIR irInfo modifier final static varType varName exprs) symbolTable currentState =
      show irInfo ++
      "private " ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState ++ ";" ++ -- ++ "=" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs) ++ ";" ++
      -- Bool to store if the value has been set yet
      "private boolean " ++ pretty varName symbolTable currentState ++ "Bool=false;" ++
      -- Create getter method
      modifier ++ " " ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState ++ "(){ if(!" ++ pretty varName symbolTable currentState ++ "Bool){" ++ pretty varName symbolTable currentState ++ "Bool=true;" ++ pretty varName symbolTable currentState ++ "=" ++ intercalate " " (map (\e -> pretty e symbolTable currentState ++ ";") exprs)  ++ "}return " ++ pretty varName symbolTable currentState ++ ";}" ++
      -- If it isn't final create a setter method
      if(not final)
        then modifier ++ " void " ++ pretty varName symbolTable currentState ++ "_(final " ++ pretty varType symbolTable currentState ++ " " ++ pretty varName symbolTable currentState ++ "){this." ++ pretty varName symbolTable currentState ++ "Bool=true;" ++ "this." ++ pretty varName symbolTable currentState ++ "=" ++ pretty varName symbolTable currentState ++ ";}"
        else ""

    pretty (IdentifierIR irInfo name) symbolTable currentState =
      show irInfo ++
      case (instanceVariableType symbolTable (currentClassName currentState) name) of
        Just varType -> do
          if instanceVariableExists symbolTable varType name
            then name ++ "()"
            else name
        Nothing -> name
    pretty (IfIR irInfo condition statement) symbolTable currentState = show irInfo ++ "if(" ++ pretty condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (ImportIR irInfo locs) symbolTable currentState = show irInfo ++ "import " ++ intercalate "." locs ++ ";"
    pretty (LambdaIR irInfo varName exprs) symbolTable currentState = show irInfo ++ "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (MainFunctionIR irInfo name annotations argTypes args returnType body) symbolTable currentState =
      show irInfo ++
      annotationString ++ "public static " ++ pretty returnType symbolTable currentState ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> pretty x symbolTable currentState) argTypes) (map (\x -> pretty x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> pretty a symbolTable currentState
              Nothing -> ""
    pretty (ModifierBlockIR irInfo exprs) symbolTable currentState = show irInfo ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (NewClassInstanceIR irInfo className args) symbolTable currentState = getDebug "NewClassInstance" ++ "new " ++ className ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ")"
    pretty (ObjectIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        show irInfo ++
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

    pretty (ObjectMethodCallIR irInfo objectName methodName args) symbolTable currentState = show irInfo ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (PrintIR irInfo exprs) symbolTable currentState = show irInfo ++ "System.out.println(" ++ pretty exprs symbolTable currentState ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    pretty (ReassignIR irInfo name value) symbolTable currentState = show irInfo ++ pretty name symbolTable currentState ++ "_(" ++ pretty value symbolTable currentState++ ");"
    pretty (ReturnIR irInfo expr) symbolTable currentState = show irInfo ++ "return " ++ pretty expr symbolTable currentState ++ ";"
    pretty (ReturnTypeIR irInfo b) symbolTable currentState = show irInfo ++ b
    pretty (SeqIR irInfo s) symbolTable currentState = show irInfo
    pretty (SkipIR irInfo) symbolTable currentState = show irInfo
    pretty (StringLiteralIR irInfo value) symbolTable currentState = show irInfo ++ "\"" ++ value ++ "\""
    pretty (SuperIR irInfo) symbolTable currentState = show irInfo ++ "super"
    pretty (SuperMethodCallIR irInfo objectName methodName args) symbolTable currentState = show irInfo ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (ThisIR irInfo) symbolTable currentState = show irInfo ++ "this"
    pretty (ThisMethodCallIR irInfo methodName args) symbolTable currentState = show irInfo ++methodName ++ "(" ++ intercalate ", " (map (\x -> pretty x symbolTable currentState) args) ++ ");"
    pretty (ThisVarIR irInfo varName) symbolTable currentState = show irInfo ++ "this." ++ pretty varName symbolTable currentState
    pretty (TraitIR irInfo packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable originalState =
        show irInfo ++
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

    pretty (TryIR irInfo exprs) symbolTable currentState = show irInfo ++ "try{" ++ intercalate " " (map (\x -> pretty x symbolTable currentState) exprs) ++ "}"
    pretty (TypeIR irInfo b) symbolTable currentState = show irInfo ++ pretty b symbolTable currentState
    pretty (WhereIR irInfo exprs) symbolTable currentState = show irInfo ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) exprs)
    pretty (WhileIR irInfo condition statement) symbolTable currentState = show irInfo ++ "while(" ++ pretty condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> pretty x symbolTable currentState) statement) ++ "}"
    pretty (Empty irInfo) symbolTable currentState = show irInfo

    -- To organise alphabetically
    pretty (VarIR irInfo v) symbolTable currentState = ""
    pretty (IntConstIR irInfo i) symbolTable currentState = ""
    pretty (NegIR irInfo aExpr) symbolTable currentState = ""
    pretty (ABinaryIR irInfo aBinOp aExpr1 aExpr2) symbolTable currentState = ""
    pretty (ParenthesisIR irInfo aExpr) symbolTable currentState = ""
    pretty (AddIR irInfo) symbolTable currentState = ""
    pretty (SubtractIR irInfo) symbolTable currentState = ""
    pretty (MultiplyIR irInfo) symbolTable currentState = ""
    pretty (DivideIR irInfo) symbolTable currentState = ""
    pretty (OpeningParenthesisIR irInfo) currentState symbolTable = ""
    pretty (ClosingParenthesisIR irInfo) currentState symbolTable = ""
    pretty (BoolConstIR irInfo  b) st cs = ""
    pretty (NotIR irInfo  n) st cs = ""
    pretty (BBinaryIR irInfo  bbinop bExpr1 bExpr2) st cs = ""
    pretty (RBinaryIR irInfo  rbinop aExpr1 aExpr2) st cs = ""
    pretty (AndIR irInfo) symbolTable currentState = ""
    pretty (OrIR irInfo) symbolTable currentState = ""
    pretty (GreaterIR irInfo) symbolTable currentState = ""
    pretty (LessIR irInfo) symbolTable currentState = ""
    pretty (GreaterEqualIR irInfo) symbolTable currentState = ""
    pretty (LessEqualIR irInfo) symbolTable currentState = ""





lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: IRNode -> Maybe [String]
extractImportStatement (ImportIR _ m) = Just m
extractImportStatement _ = Nothing

isImportStatement :: IRNode -> Bool
isImportStatement e = isJust $ extractImportStatement e
