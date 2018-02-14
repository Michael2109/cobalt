module Main where

import Compiler

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