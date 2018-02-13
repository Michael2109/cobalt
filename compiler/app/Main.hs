module Main where

import Data.List
import System.Directory
import System.FilePath.Posix

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
        else print ""
    )

  print "Compiled?"


compile :: String -> String -> IO()
compile inputFile outputFile = do
   fileData <- readFile inputFile

   print "Compiling..."
   let compiledString = parseString fileData

   print $ "Writing: " ++ outputFile
   writeFile outputFile $ compiledString


main :: IO ()
main = do
    let inputDir = "cobalt_src/"
    let outputDir = "../compiler-tester/src/java/"
    let inputFile = inputDir ++ "IndentationTest.cobalt"
    let outputFile = outputDir ++ "IndentationTest.java"

    print "Compiling directory..."
    compileDir inputDir outputDir
    print "Complete."