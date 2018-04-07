{-|
Module      : Compiler
Description : Functions for compiling and generating code.
There are functions for compiling directories or individual strings etc.
-}
module Compiler.Compiler (module Compiler.Compiler, module Util.IOUtil) where

import Control.Monad
import Data.List
import qualified Data.List.Split as Split
import Data.Text.Internal.Lazy
import System.Directory
import System.FilePath.Posix
import Text.Pretty.Simple
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack)

import AST.Block
import AST.IRNode
import AST.CodeGenNode
import Parser.ParserExecutor
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import qualified Java.Lang
import qualified Java.IO
import Util.GeneralUtil (endsWith)
import Util.IOUtil
import SymbolTable.SymbolTable

data ASTData = ASTData FilePath SymbolTable Expr
    deriving (Show)

data GeneratedCode = GeneratedCode
    { location :: FilePath -- Where the code will be written to
    , code :: B.ByteString        -- The generated code
    } deriving (Show)


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

compile :: FilePath -> [FilePath] -> String -> IO ()
compile classPath filePaths outputDir = do
    classPathFilePaths <- traverseDir classPath ""
    let filteredClassPathFilePaths = map (\filePath -> filePath) $ filter (\filePath -> ((takeFileName filePath /= ".")) && ((takeFileName filePath /= "..")) && (endsWith ".cobalt" filePath)) classPathFilePaths

    classPathFileDatas <- mapM (\filePath -> readFile $ (classPath ++ filePath)) filteredClassPathFilePaths

    let classPathAstDatas = zipWith (\filePath fileData -> generateAST filePath fileData) filteredClassPathFilePaths classPathFileDatas
    let classPathSymbolTable = SymbolTable $ concat $ map (\ sTable -> classSymbolTables sTable) $ map (extractASTSymbolTable) classPathAstDatas

    let astDatas = zipWith (\filePath fileData -> generateAST filePath fileData) filteredClassPathFilePaths classPathFileDatas
    let symbolTable = SymbolTable $ concat $ map (\ sTable -> classSymbolTables sTable) $ map (extractASTSymbolTable) astDatas

    -- Filter out only the ASTs that have been selected
    let astDatasToCompile = filter (\x -> (extractASTFilePath x) `elem` filePaths) astDatas

    let compiledCodes = map (\ astData -> GeneratedCode (extractASTFilePath astData) (compileAST (extractASTExpr astData) symbolTable)) astDatasToCompile

    sequence $ map (\compiledCode -> B.writeFile (dropExtension (outputDir ++ (location compiledCode)) ++ ".class") (code compiledCode)) compiledCodes

    return ()

generateAST :: FilePath -> String -> ASTData
generateAST inputFile code = do
   let parseResult = parseTree (Split.splitOn "/" $ (takeDirectory inputFile)) code
   let symbolTable = SymbolTable [generateClassSymbolTable parseResult]

   let ast = case parseResult of
                 Left  e -> Error
                 Right x -> x

   ASTData inputFile symbolTable ast

compileAST :: Expr -> SymbolTable -> B.ByteString
compileAST ast symbolTable = encodeClass (generate [] (pack "Test") (genCode $ genCodeGenIR (genIR ast symbolTable (CurrentState "" ""))))
