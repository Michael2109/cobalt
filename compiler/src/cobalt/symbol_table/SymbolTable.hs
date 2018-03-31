{-|
Module      : SymbolTable
Description : Contains functions used for working with symbol table.
-}
module SymbolTable where

import Data.List
import Data.Maybe

data SymbolTable = SymbolTable
  { classSymbolTables :: [ClassSymbolTable] -- All class symbol tables for all classes to be compiled
  } deriving (Eq)

data ClassSymbolTable = ClassSymbolTable
  { className       :: String -- class name
  , classType       :: ClassType
  , publicVariables :: [(String, String)] -- (variable name, variable type name) list of variable
  , methods         :: [(String, MethodSymbolTable)] -- (method name, method symbol) list of methods
  } deriving (Eq)

-- Class, Trait, Object
data ClassType = ClassType | TraitType | ObjectType | NoType
  deriving (Eq)

instance Show SymbolTable where
  show (SymbolTable classSymbolTables) = intercalate "" (map show classSymbolTables)

instance Show ClassSymbolTable where
  show (ClassSymbolTable cName cType vars methods) = show cName ++ intercalate " " (map show vars) ++ intercalate " " (map show methods)

data MethodSymbolTable = MethodSymbolTable
  { returnType :: String
  , methodArgs       :: [(String, String)] -- list of arguments
  } deriving (Show, Eq)

data CurrentState = CurrentState
  { currentClassName :: String
  , currentMethodName :: String
  } deriving (Eq)

extractReturnType :: SymbolTable -> String -> String -> String
extractReturnType symbolTable className mName = do
  let matchingMethods = map snd $ filter (\x -> mName == (fst x)) (methods (classSymbolTable) )
  if null matchingMethods
    then error ("No method found: " ++ className ++ "::" ++ mName)
    else returnType $ matchingMethods!!0
  where
    classSymbolTable = case getClassSymbolTable symbolTable className of
                         Just a -> a
                         Nothing -> error ("No class found: " ++ className)

extractMethodArgs :: SymbolTable -> String -> String -> [(String, String)]
extractMethodArgs symbolTable className mName = do
  let matchingMethods = map snd $ filter (\x -> mName == (fst x)) (methods (classSymbolTable) )
  if null matchingMethods
    then error ("No method found: " ++ className ++ "::" ++ mName)
    else methodArgs $ matchingMethods!!0
  where
    classSymbolTable = case getClassSymbolTable symbolTable className of
                         Just a -> a
                         Nothing -> error ("No class found: " ++ className)

methodExists :: SymbolTable -> String -> String -> Bool
methodExists symbolTable className mName = do
  case getClassSymbolTable symbolTable className of
    Just a -> do
      let matchingMethods = map snd $ filter (\x -> mName == (fst x)) (methods (a) )
      if null matchingMethods
        then False
        else True
    Nothing -> False

instanceVariableExists :: SymbolTable -> String -> String -> Bool
instanceVariableExists symbolTable className varName = do
  case getClassSymbolTable symbolTable className of
    Just a -> elem varName (map fst (publicVariables (a)))
    Nothing -> False

instanceVariableType :: SymbolTable -> String -> String -> Maybe(String)
instanceVariableType symbolTable className varName = do
  case getClassSymbolTable symbolTable className of
    Just a -> do
      let matchingVars = filter (\var -> varName == fst var) (publicVariables a)
      if (length matchingVars > 0)
        then Just $ snd $ matchingVars!!0
        else Nothing
    Nothing -> Nothing

methodParamExists :: SymbolTable -> String -> String -> String -> Bool
methodParamExists symbolTable className methodName varName = do
  case getClassSymbolTable symbolTable className of
    Just a ->
      do
        let methodList = map (snd) (filter (\x -> fst x == (methodName)) (methods (a)))
        if null methodList
          then False
          else if (elem (varName) (map fst (methodArgs (methodList!!0))))
            then True
            else False
    Nothing -> False

getClassSymbolTable :: SymbolTable -> String -> Maybe (ClassSymbolTable)
getClassSymbolTable symbolTable symbolTableClassName = do
  let matchingClasses = filter (\x -> symbolTableClassName == (className x)) (classSymbolTables symbolTable)
  if null matchingClasses
    then Nothing
    else Just $ matchingClasses!!0

-- todo combine if the class names are the same
combineSymbolTable :: SymbolTable -> SymbolTable -> SymbolTable
combineSymbolTable a b = SymbolTable []
  --ClassSymbolTable (className a) (publicVariables a ++ publicVariables b) (methods a ++ methods b)

combineSymbolTableList :: [SymbolTable] -> SymbolTable
combineSymbolTableList list = SymbolTable []
--do
  --if length list > 0
  --then foldl1 (\x y -> combineSymbolTable x y) list
  --else ClassSymbolTable "" [] []

combineClassSymbolTable :: ClassSymbolTable -> ClassSymbolTable -> ClassSymbolTable
combineClassSymbolTable a b = ClassSymbolTable (className a) (classType a) (publicVariables a ++ publicVariables b) (methods a ++ methods b)

combineClassSymbolTableList :: [ClassSymbolTable] -> ClassSymbolTable
combineClassSymbolTableList list = do
  if length list > 0
    then foldl1 (\x y -> combineClassSymbolTable x y) list
    else ClassSymbolTable "" ClassType [] []
