module Main where

import Data.List
import System.Directory
import System.FilePath.Posix
--import Data.List
--import Parser
--import System.Directory
import Parser

{--
main :: IO ()
main = do
    let inputDir = "cobalt/"
    let outputDir = "../compiler-tester/src/java/"
    let inputFile = inputDir ++ "TestModule.cobalt"
    let outputFile = outputDir ++ "TestModule.java"

    print $ "Reading: " ++ inputFile
    s <- readFile inputFile

    print "Compiling..."
    let compiledString = parseString s

    print "Compiling complete."
    print $ "Writing: " ++ outputFile
    writeFile outputFile $ compiledString
    --print $ parseString s

    parsePrint s

    --}

allFilesIn dir = filter (\filePath -> takeExtension filePath == ".cobalt")<$>(getDirectoryContents dir)

compileDir :: String -> String -> IO[()]
compileDir inputDir outputDir = do
  allFilesIn inputDir >>= mapM (\inputLoc -> compile (inputDir ++ inputLoc) (outputDir ++ (dropExtension inputLoc) ++ ".java"))

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

    print "Compiling directory"
    compileDir inputDir outputDir
    print "Compiled directory."
    --compile inputDir outputDir

    print $ "Reading: " ++ inputFile
    fileData <- readFile inputFile

    print "Compiling..."
    let compiledString = parseString fileData

    print "Compiling complete."
    print $ "Writing: " ++ outputFile
    writeFile outputFile $ compiledString
    --print $ parseString s

    parsePrint fileData