{-|
Module      : Compiler
Description : Functions for compiling and generating code.
There are functions for compiling directories or individual strings etc.
-}
module Compiler where

import Data.Text
import Data.Text.Internal.Lazy
import Data.List
import System.Directory
import System.FilePath.Posix
import Text.Pretty.Simple
import qualified Data.List.Split as Split

import ABBlock
import Parser
import SymbolTable
import CodeGenerator
import IOUtils

generateClassSymbolTable ast =
  case ast of
   Left  e -> ClassSymbolTable "ERROR" [] []
   Right x -> genClassSymbolTable x

compileDir :: String -> String -> SymbolTable -> IO()
compileDir inputDir outputDir symbolTable = do
  createDirectoryIfMissing True outputDir
  cleanDir (endsWith ".java") outputDir
  allFilesIn inputDir >>= mapM (\inputLoc ->
    if (endsWith "" inputLoc)
      then compileDir (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/") symbolTable
      else
        if(endsWith ".cobalt" inputLoc)
        then (compile (inputDir ++ inputLoc) (outputDir ++ (dropExtension inputLoc) ++ ".java") symbolTable)
        else putStrLn ""
    )
  putStrLn ""


compile :: String -> String -> SymbolTable -> IO()
compile inputFile outputFile classSymbolTable = do
   fileData <- readFile inputFile

   let compiledTree = parseTree (Split.splitOn "/" $ takeDirectory inputFile) fileData
   --let compiledString = parseString (Split.splitOn "/" $ takeDirectory inputFile) fileData

   pPrint "Generating symbol table"

   -- todo this should combine the found class symbol table with the current symbol table
   let symbolTable = SymbolTable [generateClassSymbolTable compiledTree]

   let generatedCode = compileAST compiledTree symbolTable
   pPrint symbolTable
   pPrint generatedCode

   --pPrint compiledTree
  -- parsePrint (Split.splitOn "/" $ takeDirectory inputFile) fileData

   writeFile outputFile generatedCode
