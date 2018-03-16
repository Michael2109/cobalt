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

compileJavaDir :: String -> String -> String -> IO()
compileJavaDir classpath inputDir outputDir = do
  let classExtension = ".class"

  createDirectoryIfMissing True outputDir
  cleanDir (endsWith classExtension) outputDir
  allFilesIn inputDir >>= mapM (\inputLoc ->
    if (takeExtension inputLoc == "")
      then compileJavaDir classpath (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
      else
        if(takeExtension inputLoc == ".java")
        then (compileJava classpath outputDir (inputDir ++ (dropExtension inputLoc) ++ ".java"))
        else putStrLn ""
    )
  putStrLn ""


compileJava :: String -> String -> String -> IO()
compileJava classpath outputDir inputFile = do
  print ("classpath " ++ classpath ++ " outputDir " ++ outputDir ++ " inputFile " ++ inputFile)
  absoluteOutputDir <- makeAbsolute outputDir
  absoluteInputFile <- makeAbsolute inputFile
  callCommand ("javac -classpath \"" ++ init classpath ++ "\" -d \"" ++ init absoluteOutputDir ++ "\" \"" ++ absoluteInputFile ++ "\"")
