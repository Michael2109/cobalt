{-|
Module      : JavaCompiler
Description : Compiles all
-}
module JavaCompiler where

import Data.Text.Internal.Lazy
import Data.List
import System.Directory
import System.FilePath.Posix
import System.Process
import IOUtils
import Utils (endsWith)

compileJavaDir :: String -> String -> String -> IO()
compileJavaDir classpath inputDir outputDir = do
  cleanDir (endsWith ".class") outputDir
  allFilesIn inputDir >>= mapM (\inputLoc ->
    if (endsWith "" inputLoc)
      then compileJavaDir classpath (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
      else
        if(endsWith ".java" inputLoc)
        then (compileJava classpath outputDir (inputDir ++ (dropExtension inputLoc) ++ ".java"))
        else putStrLn ""
    )
  putStrLn ""


compileJava :: String -> String -> String -> IO()
compileJava classpath outputDir inputFile = do
  print ("classpath " ++ classpath ++ " outputDir " ++ outputDir ++ " inputFile " ++ inputFile)
  absoluteOutputDir <- makeAbsolute outputDir
  absoluteInputFile <- makeAbsolute inputFile
  let command = "javac -classpath \"" ++ init classpath ++ "\" -d \"" ++ init absoluteOutputDir ++ "\" \"" ++ absoluteInputFile ++ "\""
  putStrLn command
  callCommand command
