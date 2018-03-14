module SymbolTable where

import Data.List
import Data.Maybe

data SymbolTable
  = SymbolTable
    {
       classSymbolTables :: [ClassSymbolTable] -- All class symbol tables for all classes to be compiled
    }
    deriving (Eq)

data ClassSymbolTable
    = ClassSymbolTable
      {
          className       :: String -- class name
        , publicVariables :: [(String, String)] -- (variable name, variable type name) list of variable
        , methods         :: [(String, MethodSymbolTable)] -- (method name, method symbol) list of methods
      }
      deriving (Eq)


instance Show SymbolTable where
    show (SymbolTable classSymbolTables) = intercalate "" (map show classSymbolTables)

instance Show ClassSymbolTable where
    show (ClassSymbolTable cName vars methods) = show cName ++ intercalate " " (map show vars) ++ intercalate " " (map show methods)

data MethodSymbolTable = MethodSymbolTable {
      returnType :: String
    , methodArgs       :: [(String, String)] -- list of arguments
    }
    deriving (Show, Eq)

data CurrentState
  = CurrentState {
    currentClassName :: String,
    currentMethodName :: String
  }
  deriving (Eq)


extractReturnType :: SymbolTable -> String -> String -> String
extractReturnType symbolTable className mName = do
  let matchingMethods = map snd $ filter (\x -> mName == (fst x)) (methods (getClassSymbolTable symbolTable className) )
  if null matchingMethods
    then error ("No method found: " ++ className ++ "::" ++ mName)
    else returnType $ matchingMethods!!0


extractMethodArgs :: SymbolTable -> String -> String -> [(String, String)]
extractMethodArgs symbolTable className mName = do
  let matchingMethods = map snd $ filter (\x -> mName == (fst x)) (methods (getClassSymbolTable symbolTable className) )
  if null matchingMethods
    then error ("No method found: " ++ className ++ "::" ++ mName)
    else methodArgs $ matchingMethods!!0

methodExists :: SymbolTable -> String -> String -> Bool
methodExists symbolTable className mName = do
  let matchingMethods = map snd $ filter (\x -> mName == (fst x)) (methods (getClassSymbolTable symbolTable className) )
  if null matchingMethods
    then False
    else True

instanceVariableExists :: SymbolTable -> String -> String -> Bool
instanceVariableExists symbolTable className varName = do
  elem varName (map fst (publicVariables (getClassSymbolTable symbolTable className)))

methodParamExists :: SymbolTable -> String -> String -> String -> Bool
methodParamExists symbolTable className methodName varName = do
  let methodList = map (snd) (filter (\x -> fst x == (methodName)) (methods (getClassSymbolTable symbolTable className)))
  if null methodList
    then False
    else if (elem (varName) (map fst (methodArgs (methodList!!0))))
      then True
      else False

getClassSymbolTable :: SymbolTable -> String -> ClassSymbolTable
getClassSymbolTable symbolTable symbolTableClassName = do
  let matchingClasses = filter (\x -> symbolTableClassName == (className x)) (classSymbolTables symbolTable)
  if null matchingClasses
    then error ("Class not found: " ++ symbolTableClassName)
    else matchingClasses!!0


