module Main where

import Data.List
import Parser

main :: IO ()
main = do
    let inputDir = "cobalt/"
    --let outputDir = "C:/Users/Michael/Desktop/CompilerTester/src/java/"
    let outputDir = "../compiler-tester/src/java/"
    let inputFile = inputDir ++ "TestModule.cobalt"
    let outputFile = outputDir ++ "TestModule.java"

    s <- readFile inputFile
    let compiledString = parseString s

    writeFile outputFile $ compiledString
    --print $ parseString s
    parsePrint s