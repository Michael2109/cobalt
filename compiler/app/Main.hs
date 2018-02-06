module Main where

import Data.List
import System.Directory (getDirectoryContents)
--import Data.List
--import Parser
--import System.Directory
import IndentTest

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



--compileDir :: String -> IO()
--compileDir directory outputDirectory = do
--  filePaths <- getDirectoryContents directory
 -- mapM (comp) map (\p -> outputDirectory ++ p) filePaths

compile :: String -> String -> IO()
compile inputDir outputDir = do
   --getDirectoryContents inputDir
   print "Test"




main :: IO ()
main = do
    let inputDir = "cobalt_src/"
    let outputDir = "../compiler-tester/src/java/"
    let inputFile = inputDir ++ "IndentationTest.cobalt"
    let outputFile = outputDir ++ "IndentationTest.java"

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