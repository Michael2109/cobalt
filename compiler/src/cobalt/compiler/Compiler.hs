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

import Parser

--allFilesIn dir = filter (\filePath -> takeExtension filePath == ".cobalt")<$>(getDirectoryContents dir)
allFilesIn dir = getDirectoryContents dir


compileDir :: String -> String -> IO()
compileDir inputDir outputDir = do
  createDirectoryIfMissing False outputDir
  allFilesIn inputDir >>= mapM (\inputLoc ->
    if (takeExtension inputLoc == "")
      then compileDir (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
      else
        if(takeExtension inputLoc == ".cobalt")
        then (compile (inputDir ++ inputLoc) (outputDir ++ (dropExtension inputLoc) ++ ".java"))
        else putStrLn ""
    )
  putStrLn ""


compile :: String -> String -> IO()
compile inputFile outputFile = do
   fileData <- readFile inputFile

   let compiledTree = parseTree (Split.splitOn "/" $ takeDirectory inputFile) fileData
   let compiledString = parseString (Split.splitOn "/" $ takeDirectory inputFile) fileData
   putStrLn $ "Compiling: " ++ inputFile
   putStrLn ""
   pPrint compiledTree
   putStrLn $ "Compiled:" ++ outputFile
   putStrLn ""
   putStrLn ""

   writeFile outputFile compiledString

