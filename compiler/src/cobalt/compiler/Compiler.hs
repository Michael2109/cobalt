{-|
Module      : Compiler
Description : Functions for compiling and generating code.
There are functions for compiling directories or individual strings etc.
-}
module Compiler where

import Control.Monad
import Data.Text.Internal.Lazy
import Data.List
import System.Directory
import System.FilePath.Posix
import Text.Pretty.Simple
import qualified Data.List.Split as Split

import ABBlock
import Block
import Parser
import SymbolTable
import CodeGenerator


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
   Left  e -> ClassSymbolTable "ERROR" [] []
   Right x -> genClassSymbolTable x

compile :: String -> String -> IO ()
compile inputDir outputDir = do

  generateMissingDirectories inputDir outputDir
  filePaths <- traverseDir inputDir ""

  let filteredFilePaths = filter (\filePath -> ((takeFileName filePath /= ".")) && ((takeFileName filePath /= "..")) && (takeExtension filePath == ".cobalt")) filePaths

  fileDatas <- mapM (\filePath -> readFile $ (inputDir ++ filePath)) filteredFilePaths

  let astDatas = zipWith (\filePath fileData -> generateAST inputDir filePath fileData) filteredFilePaths fileDatas

  -- Combines all class symbol tables for all ASTs
  let symbolTable = SymbolTable $ concat $ map (\ sTable -> classSymbolTables sTable) $ map (extractASTSymbolTable) astDatas

  let compiledCodes = map (\ astData -> GeneratedCode (extractASTFilePath astData) (compileAST (extractASTExpr astData) symbolTable)) astDatas

  sequence $ map (\ compiledCode -> writeFile (dropExtension (outputDir ++ location compiledCode) ++ ".java") (code compiledCode)) compiledCodes

  return ()


allFilesIn dir = getDirectoryContents dir

-- | Traverse from 'top' directory and return all the files by
-- filtering out the 'exclude' predicate.
traverseDir :: FilePath -> FilePath -> IO [FilePath]
traverseDir inputDir top = do
  ds <- getDirectoryContents $ inputDir ++ top
  paths <- forM (ds) $ \d -> do
    let path = top </> d
    if takeExtension (inputDir ++ path) == ""
      then traverseDir (inputDir) path
      else return [path]
  return (concat paths)



generateMissingDirectories :: String -> String -> IO()
generateMissingDirectories inputDir outputDir = do
  allFilesIn inputDir >>= mapM (\inputLoc ->
    do
      createDirectoryIfMissing True outputDir
      when (takeExtension inputLoc == "") $ do
        generateMissingDirectories (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
    )
  return ()


generateAST :: FilePath -> FilePath -> String -> ASTData
generateAST inputDir inputFile code = do

   let parseResult = parseTree (Split.splitOn "/" $ (inputDir ++ (takeDirectory inputFile))) code
   let symbolTable = SymbolTable [generateClassSymbolTable parseResult]

   let ast = case parseResult of
               Left  e -> Error
               Right x -> x

   ASTData inputFile symbolTable ast