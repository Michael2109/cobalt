{-|
Module      : Block
Description : Data types that store general expressions.
These are used to store the data and specify how the code is generated.
-}

module Block where

import Data.List
import Text.Show.Functions
import Data.Char
import Data.Maybe

import ABBlock
import SymbolTable

debug :: Bool
debug = False

getDebug :: String -> String
getDebug message = (if debug then "<" ++ message ++ "> " else "")


-- Statements
data Expr
  = Seq [Expr]
  | Import {locs ::[String]}
  | GlobalVar String Bool Bool Expr Expr [Expr]
  | MainFunction {name ::String, annotations :: (Maybe Expr), argTypes:: [Expr], args::[Expr], returnType::Expr, body::[Expr]}
  | Function String (Maybe Expr) [Expr] [Expr] Expr Bool [Expr]
  | Constructor String [Expr] [Expr] [Expr]
  | FunctionCall String [Expr]
  | Type Expr
  | ValueType String
  | Argument String
  | ArgumentType String
  | ReturnType String
  | AssignArith Bool Expr String Expr
  | ArithExpr AExpr
  | ArrayType String
  | ArrayAppend [Expr]
  | Assign Expr Expr Expr
  | Reassign String Expr
  | If BExpr [Expr]
  | ElseIf BExpr [Expr]
  | Else [Expr]
  | Try [Expr]
  | Catch (Maybe [Expr]) [Expr]
  | While Expr [Expr]
  | Print Expr
  | Return Expr
  | ArrayValues [String]
  | ArrayDef String String
  | ArrayAssignment Expr Expr
  | ArrayElementSelect String
  | Where [Expr]
  | StringLiteral String
  | Data String [Expr]
  | DataElement String String [String] [String]
  | DataInstance Expr [Expr]
  | SuperMethodCall String String [Expr]
  | ObjectMethodCall String String [Expr]
  | ThisMethodCall String [Expr]
  | NewClassInstance String [Expr]
  | ClassVariable String String
  | BooleanExpr BExpr
  | Identifier String
  | Annotation String
  | ModifierBlock [Expr]
  | This
  | Super
  | Lambda String [Expr]
  | ClassParam {varType :: Expr, varName :: Expr}
  | Skip

  -- Module specific
  | Object [String]String (Maybe [Expr]) (Maybe String) (Maybe String) [Expr] [Expr] [Expr] [Expr]

  -- Class specific
  | Class [String]String (Maybe [Expr]) (Maybe String) (Maybe String) [Expr] [Expr] [Expr] [Expr]

instance ErrorCheck Expr where
  errorCheck (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = "An error occurred"
  errorCheck (_) = "<Unimplemented error check>"


instance SymbolTableGen Expr where
  genSymbolTable (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) = combineSymbolTable (combineSymbolTable (ClassSymbolTable name [] []) (foldl1 (\x y -> combineSymbolTable x y) (map genSymbolTable modifierBlocks))) (foldl1 (\x y -> combineSymbolTable x y) (map genSymbolTable bodyArray))
  genSymbolTable (ModifierBlock exprs) =  foldl1 (\x y -> combineSymbolTable x y) (map genSymbolTable exprs)
  genSymbolTable (GlobalVar modifier final static varType varName exprs) =  ClassSymbolTable "" [(show varName, show varType)] []
  genSymbolTable (Function name annotations argTypes args returnType static body) = ClassSymbolTable "" [] [(name, (MethodSymbolTable (show returnType) (zip (map show args) (map show argTypes))))]
  genSymbolTable (_) = ClassSymbolTable "" [] []

instance Show Expr where
  show (GlobalVar modifier final static varType varName exprs) = ""
  show (Identifier name) = name
  show (Type e) = show e
  show (Argument e) = e
  show (ArgumentType e) = e
  show (_) = "<Unknown show>"

combineSymbolTable :: ClassSymbolTable -> ClassSymbolTable -> ClassSymbolTable
combineSymbolTable a b = ClassSymbolTable (className a) (publicVariables a ++ publicVariables b) (methods a ++ methods b)

combineSymbolTableList :: [ClassSymbolTable] -> ClassSymbolTable
combineSymbolTableList list = foldl1 (\x y -> combineSymbolTable x y) list

instance CodeGen Expr where
    genCode (Class packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable currentState =
        getDebug "Class" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) modifierBlocks) ++

        intercalate " " (map (\x -> "private final " ++ genCode x symbolTable currentState ++ ";") paramList) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> genCode x symbolTable currentState) paramList) ++"){" ++

        intercalate " " (map (\x -> "this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";") paramList) ++

        intercalate " " (map (\x -> genCode x symbolTable currentState) constructorExprs) ++ "}" ++

        --intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          paramList = case params of
              Just a -> a
              Nothing -> []
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              Just a -> "implements " ++ a
              Nothing -> ""
    genCode (Object packageLocs name params parent interfaces imports modifierBlocks constructorExprs bodyArray) symbolTable currentState =
        getDebug "Module" ++
        (if(length packageLocs > 0)
          then "package " ++ (intercalate "." packageLocs) ++ ";"
          else "")
        ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) imports) ++
        "public final class " ++ name ++ " " ++ extendSection ++ " " ++ implementSection ++ "{\n" ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) modifierBlocks) ++

        intercalate " " (map (\x -> "private final " ++ genCode x symbolTable currentState ++ ";") paramList) ++

        -- Constructor
        "public " ++ name ++ "("++ intercalate ", " (map (\x -> genCode x symbolTable currentState) paramList) ++"){" ++

        intercalate " " (map (\x -> "this." ++ genCode (varName x) symbolTable currentState ++ "=" ++ genCode (varName x) symbolTable currentState ++ ";") paramList) ++

        intercalate " " (map (\x -> genCode x symbolTable currentState) constructorExprs) ++ "}" ++

        --intercalate "\n" (map (\x -> "final " ++ (id $ last (locs x)) ++ " " ++ lowerString (id $ last (locs x)) ++ "= new " ++ (id $ last (locs x)) ++ "();") imports) ++
        intercalate "\n" (map (\x -> genCode x symbolTable currentState) (filter (not . isImportStatement) bodyArray))  ++
        "}"
        where
          paramList = case params of
              Just a -> a
              Nothing -> []
          extendSection = case parent of
              Just a -> "extends " ++ a
              Nothing -> ""
          implementSection = case interfaces of
              Just a -> "implements " ++ a
              Nothing -> ""
    genCode (Import locs) symbolTable currentState = getDebug "Import" ++ "import " ++ intercalate "." locs ++ ";"
    genCode (GlobalVar modifier final static varType varName exprs) symbolTable currentState =
      getDebug "GlobalVar" ++
      "private " ++ (if (static) then "static " else "") ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState ++ ";" ++ -- ++ "=" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs) ++ ";" ++
      -- Bool to store if the value has been set yet
      "private " ++ (if (static) then "static " else "") ++ "boolean " ++ genCode varName symbolTable currentState ++ "Bool=false;" ++
      -- Create getter method
      modifier ++ (if (static) then " static " else "") ++ " " ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState ++ "(){ if(!" ++ genCode varName symbolTable currentState ++ "Bool){" ++ genCode varName symbolTable currentState ++ "Bool=true;" ++ genCode varName symbolTable currentState ++ "=" ++ intercalate " " (map (\e -> genCode e symbolTable currentState ++ ";") exprs)  ++ "}return " ++ genCode varName symbolTable currentState ++ ";}" ++
      -- If it isn't final create a setter method
      if(not final)
        then modifier ++ " void " ++ genCode varName symbolTable currentState ++ "_(final " ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState ++ "){this." ++ genCode varName symbolTable currentState ++ "Bool=true;" ++ "this." ++ genCode varName symbolTable currentState ++ "=" ++ genCode varName symbolTable currentState ++ ";}"
        else ""

    genCode (Constructor name argTypes args body) symbolTable currentState = getDebug "Constructor" ++ "public " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable currentState) argTypes) (map (\x -> genCode x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) body) ++ "}"
    genCode (Function name annotations argTypes args returnType static body) symbolTable currentState =
      getDebug "Function" ++
      annotationString ++ " public " ++ (if(static) then "static " else "") ++ genCode returnType symbolTable (CurrentState name) ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable (CurrentState name)) argTypes) (map (\x -> genCode x symbolTable (CurrentState name)) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable (CurrentState name)) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> genCode a symbolTable currentState
              Nothing -> ""
    genCode (MainFunction name annotations argTypes args returnType body) symbolTable currentState =
      getDebug "MainFunction" ++
      annotationString ++ "public static " ++ genCode returnType symbolTable currentState ++ " " ++ name ++ "("++ intercalate ", " (zipWith (\x y -> x ++ " " ++ y) (map (\x -> genCode x symbolTable currentState) argTypes) (map (\x -> genCode x symbolTable currentState) args)) ++"){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) body) ++ "}"
        where
          annotationString = case annotations of
              Just a -> genCode a symbolTable currentState
              Nothing -> ""
    genCode (FunctionCall name exprs) symbolTable currentState = getDebug "FunctionCall" ++ name ++ "(" ++ (intercalate ", " (map (\x -> genCode x symbolTable currentState) exprs)) ++ ");"
    genCode (Type b) symbolTable currentState = getDebug "Type" ++ genCode b symbolTable currentState
    genCode (Argument b) symbolTable currentState = do
        let methodList = map (snd) (filter (\x -> fst x == (method currentState)) (methods symbolTable))
        if(length methodList > 0)
        then if (elem b (map fst (methodArgs (methodList!!0)))) then  getDebug "Argument" ++ b else  getDebug "Argument" ++ (b ++ "()")
        else  getDebug "Argument" ++ b
    genCode (ArgumentType b) symbolTable currentState = getDebug "ArgumentType" ++ b
    genCode (ReturnType b) symbolTable currentState = getDebug "ReturnType" ++ b
    genCode (AssignArith mutable vType name value) symbolTable currentState = getDebug "AssignArith" ++ (if mutable then "" else "final ") ++ genCode vType symbolTable currentState ++ " " ++ name ++ "=" ++ genCode value symbolTable currentState ++ ";"
    genCode (ArithExpr aExpr) symbolTable currentState = getDebug "ArithExpr" ++ genCode aExpr symbolTable currentState
    genCode (Assign vType name value) symbolTable currentState = getDebug "Assign" ++ genCode vType symbolTable currentState ++ " " ++ genCode name symbolTable currentState ++ "=" ++ genCode value symbolTable currentState ++ ";"
    genCode (Reassign name value) symbolTable currentState = getDebug "Reassign" ++ name ++ "_(" ++ genCode value symbolTable currentState++ ");"
    genCode (If condition statement) symbolTable currentState = getDebug "If" ++ "if(" ++ genCode condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (ElseIf condition statement) symbolTable currentState = getDebug "ElseIf" ++ " else if(" ++ genCode condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (Else statement) symbolTable currentState = getDebug "Else" ++ " else {\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (Try exprs) symbolTable currentState = getDebug "Try" ++ "try{" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs) ++ "}"
    genCode (Catch params exprs) symbolTable currentState = getDebug "Catch" ++ "catch(" ++  intercalate ", " (map (\x -> "final " ++ genCode x symbolTable currentState) paramList)  ++ "){" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs) ++ "}"
      where
        paramList = case params of
            Just a -> a
            Nothing -> []
    genCode (While condition statement) symbolTable currentState = getDebug "While" ++ "while(" ++ genCode condition symbolTable currentState ++ "){\n" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) statement) ++ "}"
    genCode (Skip) symbolTable currentState = getDebug "Skip" ++ "[skip]"
    genCode (Seq s) symbolTable currentState = getDebug "Seq" ++ "[seq]"
    genCode (Return expr) symbolTable currentState = getDebug "Return" ++ "return " ++ genCode expr symbolTable currentState ++ ";"
    genCode (Print exprs) symbolTable currentState = getDebug "Print" ++ "System.out.println(" ++ genCode exprs symbolTable currentState ++ ");" --"System.out.println(" ++ intercalate " " (map show exprs) ++ ");"
    genCode (ArrayType arrType) symbolTable currentState = getDebug "ArrayType" ++ arrType ++ "[]"
    genCode (ArrayDef arrType name) symbolTable currentState = getDebug "ArrayDef" ++ arrType ++ "[] " ++ name ++ "="
    genCode (ArrayValues exprs) symbolTable currentState = getDebug "ArrayValues" ++ "{" ++ intercalate ", " exprs ++ "};"
    genCode (ArrayAssignment arr values) symbolTable currentState = getDebug "ArrayAssignment" ++ genCode arr symbolTable currentState ++ genCode values symbolTable currentState
    genCode (ArrayAppend arrays) symbolTable currentState = getDebug "ArrayAppend" ++ intercalate "" (map (\arr -> "") arrays)
    genCode (ArrayElementSelect i) symbolTable currentState = getDebug "ArrayElementSelect" ++ "[" ++ i ++ "];"
    genCode (Where exprs) symbolTable currentState = getDebug "Where" ++ intercalate "\n" (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (StringLiteral value) symbolTable currentState = getDebug "StringLiteral" ++ "\"" ++ value ++ "\""
    genCode (Data name exprs) symbolTable currentState = getDebug "Data" ++ "class " ++ name ++ "{}" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (DataElement superName name argTypes args) symbolTable currentState = getDebug "DataElement" ++ "final class " ++ name ++ " extends "++ superName ++ " { " ++
      intercalate " "(zipWith (\x y -> "final " ++ x ++ " " ++ y ++ ";") argTypes args) ++ " public " ++ name ++ "(" ++ intercalate ", " (zipWith (\x y -> "final " ++ x ++ " " ++ y) argTypes args) ++
      "){" ++
      intercalate " " (map (\x ->"this." ++ x ++ "=" ++ x ++ ";") args) ++
      "} }"
    genCode (DataInstance typeName args) symbolTable currentState = getDebug "DataInstance" ++ "new " ++ genCode typeName symbolTable currentState ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (ThisMethodCall methodName args) symbolTable currentState = getDebug "ThisMethodCall" ++methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (SuperMethodCall objectName methodName args) symbolTable currentState = getDebug "SuperMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (ObjectMethodCall objectName methodName args) symbolTable currentState = do
      let methodList = map (snd) (filter (\x -> fst x == (method currentState)) (methods symbolTable))
      if(length methodList > 0)
      then (if (elem objectName (map fst (methodArgs (methodList!!0))) || not (elem objectName (map (fst) $ publicVariables symbolTable ))) then  getDebug "ObjectMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");" else getDebug "Argument" ++ (objectName ++ "()") ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");")
      else if(not (elem objectName (map (fst) $ publicVariables symbolTable ))) then getDebug "ObjectMethodCall" ++ objectName ++ "." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");" else getDebug "ObjectMethodCall" ++ objectName ++ "()." ++ methodName ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ");"
    genCode (NewClassInstance className args) symbolTable currentState = getDebug "NewClassInstance" ++ "new " ++ className ++ "(" ++ intercalate ", " (map (\x -> genCode x symbolTable currentState) args) ++ ")"
    genCode (ClassVariable className varName) symbolTable currentState = getDebug "ClassVariable" ++className ++ "." ++ varName
    genCode (BooleanExpr expr) symbolTable currentState = getDebug "BooleanExpr" ++ genCode expr symbolTable currentState
    genCode (Identifier name) symbolTable currentState = getDebug "Identifier" ++ name
    genCode (Annotation name) symbolTable currentState = getDebug "Annotation" ++ "@" ++ name
    genCode (ModifierBlock exprs) symbolTable currentState = getDebug "ModifierBlock" ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (This) symbolTable currentState = getDebug "This" ++ "this"
    genCode (Super) symbolTable currentState = getDebug "Super" ++ "super"
    genCode (Lambda varName exprs) symbolTable currentState = getDebug "Lambda" ++ "<LAMBDA " ++ varName ++ " " ++ intercalate " " (map (\x -> genCode x symbolTable currentState) exprs)
    genCode (ClassParam varType varName) symbolTable currentState = getDebug "ClassParam" ++ genCode varType symbolTable currentState ++ " " ++ genCode varName symbolTable currentState
    genCode (_) symbolTable currentState = "<unknown>"

lowerString str = [ toLower loweredString | loweredString <- str]

extractImportStatement :: Expr -> Maybe [String]
extractImportStatement (Import m) = Just m
extractImportStatement _ = Nothing

isImportStatement :: Expr -> Bool
isImportStatement e = isJust $ extractImportStatement e
