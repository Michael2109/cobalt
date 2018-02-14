module Main where

import Data.Text
import Data.List
import System.Directory
import System.FilePath.Posix
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

   let compiledString = parseString (Split.splitOn "/" $ takeDirectory inputFile) fileData
   putStrLn $ "Compiling: " ++ inputFile
   putStrLn ""
   print compiledString
   putStrLn $ "Compiled:" ++ outputFile
   putStrLn ""
   putStrLn ""

   writeFile outputFile $ show compiledString


main :: IO ()
main = do
    let inputDir = "cobalt_src/"
    let outputDir = "../compiler-tester/src/java/"
    let inputFile = inputDir ++ "IndentationTest.cobalt"
    let outputFile = outputDir ++ "IndentationTest.java"

    putStrLn "Compiling directory..."
    putStrLn ""
    compileDir inputDir outputDir
    putStrLn "Complete."