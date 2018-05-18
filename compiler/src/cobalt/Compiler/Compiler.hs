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

import AST.AST
import AST.CodeGen
import Parser.ParserExecutor
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import qualified Java.Lang
import qualified Java.IO
import Util.CommandLineUtil (CommandLineArgument (..))
import Util.GeneralUtil (endsWith)
import Util.IOUtil
import SymbolTable.SymbolTable

data ASTData = ASTData FilePath SymbolTable Module
    deriving (Show)

data GeneratedCode = GeneratedCode
    { location :: FilePath -- Where the code will be written to
    , code :: B.ByteString        -- The generated code
    } deriving (Show)

extractASTFilePath :: ASTData -> FilePath
extractASTFilePath (ASTData filePath _ _) = filePath

extractASTSymbolTable :: ASTData -> SymbolTable
extractASTSymbolTable (ASTData _ symbolTable _) = symbolTable

extractASTExpr :: ASTData -> Module
extractASTExpr (ASTData _ _ expr) = expr

generateModelSymbolTable ast = error "Needs reimplementing"
    {--
    case ast of
        Left  e -> ModelSymbolTable "ERROR" NoType [] [] []
        Right x -> genModelSymbolTable x --}

compile :: [CommandLineArgument] -> FilePath -> [FilePath] -> String -> IO ()
compile commandLineArguments classPath filePaths outputDir = do

    classPathFilePaths <- traverseDir classPath ""

    let filteredClassPathFilePaths = map (\filePath -> filePath) $ filter (\filePath -> ((takeFileName filePath /= ".")) && ((takeFileName filePath /= "..")) && (endsWith ".cobalt" filePath)) classPathFilePaths

    classPathFileDatas <- mapM (\filePath -> readFile $ (classPath ++ filePath)) filteredClassPathFilePaths

    let classPathAstDatas = zipWith (\filePath fileData -> generateAST filePath fileData) filteredClassPathFilePaths classPathFileDatas
    let classPathSymbolTable = SymbolTable $ concat $ map (\ sTable -> modelSymbolTables sTable) $ map (extractASTSymbolTable) classPathAstDatas

    let astDatas = zipWith (\filePath fileData -> generateAST filePath fileData) filteredClassPathFilePaths classPathFileDatas

    let symbolTable = SymbolTable $ concat $ map (\ sTable -> modelSymbolTables sTable) $ map (extractASTSymbolTable) astDatas

    -- Filter out only the ASTs that have been selected
    let astDatasToCompile = filter (\x -> (extractASTFilePath x) `elem` filePaths) astDatas

    -- Print out the AST when in debug
    when (elem (DebugMode) commandLineArguments) $ pPrint astDatasToCompile

    let compiledCodes = map (\ astData -> GeneratedCode (extractASTFilePath astData) (compileAST (takeFileName $ dropExtensions (extractASTFilePath astData)) (extractASTExpr astData) symbolTable)) astDatasToCompile

    sequence $ map (\compiledCode -> B.writeFile (dropExtension (outputDir ++ (location compiledCode)) ++ ".class") (code compiledCode)) compiledCodes

    return ()

generateAST :: FilePath -> String -> ASTData
generateAST inputFile code = do
    let parseResult = parseTree (Split.splitOn "/" $ (takeDirectory inputFile)) code
    let symbolTable = SymbolTable []
    let ast = case parseResult of
                  Left  e -> error $ show e
                  Right x -> x

    ASTData inputFile symbolTable ast

compileAST :: String -> Module -> SymbolTable -> B.ByteString
compileAST moduleName ast symbolTable = encodeClass (generate [] (pack moduleName) (genCode ast))
