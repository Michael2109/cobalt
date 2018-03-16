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

  --generateMissingDirectories inputDir outputDir
  filePaths <- traverseDir inputDir ""

  let filteredFilePaths = filter (\filePath -> ((takeFileName filePath /= ".")) && ((takeFileName filePath /= "..")) && (takeExtension filePath == ".cobalt")) filePaths
  --let inputFilePaths = map (\filePath -> inputDir ++ filePath) filteredFilePaths
  --let outputFilePaths = map (\filePath -> outputDir ++ filePath) filteredFilePaths

  print "Input paths"
  pPrint inputFilePaths

  print "Output paths"
  pPrint outputFilePaths

  fileDatas <- mapM (readFile) inputFilePaths


  let astDatas = zipWith (\filePath fileData -> generateAST (inputDir ++ filePath fileData)) filteredFilePaths fileDatas

  -- Combines all class symbol tables
  let symbolTable = SymbolTable $ concat $ map (\ sTable -> classSymbolTables sTable) $ map (extractASTSymbolTable) astDatas

  --pPrint symbolTable

  let compiledCodes = map (\ astData -> GeneratedCode (extractASTFilePath astData) (compileAST (extractASTExpr astData) symbolTable)) astDatas

  pPrint compiledCodes

  map (\ compiledCode -> writeFile (outputDir ++ location compiledCode) (code compiledCode)) compiledCodes

  return ()


--inputToOutputFilePath inputFile outputDir =

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


generateAST :: FilePath -> String -> ASTData
generateAST inputFile code = do

   let parseResult = parseTree (Split.splitOn "/" $ takeDirectory inputFile) code
   let symbolTable = SymbolTable [generateClassSymbolTable parseResult]

   let ast = case parseResult of
               Left  e -> Error
               Right x -> x

   ASTData inputFile symbolTable ast

{--
generateASTs :: String -> String -> SymbolTable -> IO [Expr]
generateASTs inputDir outputDir symbolTable = do
  createDirectoryIfMissing True outputDir
  allFilesIn inputDir >>= foldM (\inputLoc ->
    if (takeExtension inputLoc == "")
      then compileDir (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/") symbolTable
      else
        if(takeExtension inputLoc == ".cobalt")
        then [(compile (inputDir ++ inputLoc) (outputDir ++ (dropExtension inputLoc) ++ ".java") symbolTable)]
        else return ()
    )
  return ()


generateAST :: String -> String -> SymbolTable -> IO Expr
generateAST inputFile outputFile classSymbolTable = do
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
--}

{--


compileDir :: String -> String -> SymbolTable -> IO ()
compileDir inputDir outputDir symbolTable = do
  createDirectoryIfMissing True outputDir
  allFilesIn inputDir >>= mapM (\inputLoc ->
    if (takeExtension inputLoc == "")
      then compileDir (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/") symbolTable
      else
        if(takeExtension inputLoc == ".cobalt")
        then (compile (inputDir ++ inputLoc) (outputDir ++ (dropExtension inputLoc) ++ ".java") symbolTable)
        else putStrLn ""
    )
  putStrLn ""


compile :: String -> String -> SymbolTable -> IO ()
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

--}