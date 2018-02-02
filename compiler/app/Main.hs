module Main where

--import Data.List
--import Parser
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

main :: IO ()
main = do
    let inputDir = "cobalt/"
    let outputDir = "../compiler-tester/src/java/"
    let inputFile = inputDir ++ "IndentationTest.cobalt"
    let outputFile = outputDir ++ "IndentationTest.java"

    print $ "Reading: " ++ inputFile
    s <- readFile inputFile

    --print "Compiling..."
    --let compiledString = parseString s

    print "Compiling complete."
    print $ "Writing: " ++ outputFile
    --writeFile outputFile $ compiledString
    --print $ parseString s

    parsePrint s