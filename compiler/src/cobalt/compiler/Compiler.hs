{-|
Module      : Compiler
Description : Functions for compiling and generating code.
There are functions for compiling directories or individual strings etc.
-}
module Compiler (module Compiler, module IOUtils) where

import Control.Monad
import Data.Text.Internal.Lazy
import Data.List
import System.Directory
import System.FilePath.Posix
import Text.Pretty.Simple
import qualified Data.List.Split as Split

import Block
import Parser
import SymbolTable
import CodeGenerator
import IOUtils
import Utils (endsWith)
import IRNode


data ASTData = ASTData FilePath SymbolTable Expr
  deriving (Show)


extractASTFilePath :: ASTData -> FilePath
extractASTFilePath (ASTData filePath _ _) = filePath

extractASTSymbolTable :: ASTData -> SymbolTable
extractASTSymbolTable (ASTData _ symbolTable _) = symbolTable

extractASTExpr :: ASTData -> Expr
extractASTExpr (ASTData _ _ expr) = expr


generateClassSymbolTable ast =
  case ast of
   Left  e -> ClassSymbolTable "ERROR" NoType [] []
   Right x -> genClassSymbolTable x

compile :: [String] -> String -> IO ()
compile filePaths outputDir = do
  --Allows only files ending with ".cobalt" from those specified in commandline
  --it can be made more permissive by omittinig following filtering
  let filteredFilePaths = filter (\filePath -> ((takeFileName filePath /= ".")) && ((takeFileName filePath /= "..")) && (endsWith ".cobalt" filePath)) filePaths
  fileDatas <- mapM (\filePath -> readFile $ (filePath)) filteredFilePaths

  let astDatas = zipWith (\filePath fileData -> generateAST filePath fileData) filteredFilePaths fileDatas
  -- Combines all class symbol tables for all ASTs
  let symbolTable = SymbolTable $ concat $ map (\ sTable -> classSymbolTables sTable) $ map (extractASTSymbolTable) astDatas

  let compiledCodes = map (\ astData -> GeneratedCode (extractASTFilePath astData) (compileAST (extractASTExpr astData) symbolTable)) astDatas
  -- the output class files are stored in single directory without creating subdirectories reflecting the source filepaths. Might cause problems with duplicates
  -- (javac seems to do this too, unless compiling a package). Also, currently class, object and trait parsers use inputFilePath for package info.
  sequence $ map (\ compiledCode -> writeFile (dropExtension (outputDir ++ (takeFileName (location compiledCode))) ++ ".java") (code compiledCode)) compiledCodes

  pPrint symbolTable

  return ()

generateAST :: FilePath -> String -> ASTData
generateAST inputFile code = do

   let parseResult = parseTree (Split.splitOn "/" $ (takeDirectory inputFile)) code
   let symbolTable = SymbolTable [generateClassSymbolTable parseResult]

   let ast = case parseResult of
               Left  e -> Error
               Right x -> x

   ASTData inputFile symbolTable ast
