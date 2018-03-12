{-|
Module      : JavaCompiler
Description : Compiles all
-}
module JavaCompiler where

import Data.Text
import Data.Text.Internal.Lazy
import Data.List
import System.Directory
import System.FilePath.Posix
import System.Process


allFilesIn dir = getDirectoryContents dir

compileJavaDir :: String -> String -> String -> IO()
compileJavaDir classpath inputDir outputDir = do
  createDirectoryIfMissing True outputDir
  allFilesIn inputDir >>= mapM (\inputLoc ->
    if (takeExtension inputLoc == "")
      then compileJavaDir classpath (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
      else
        if(takeExtension inputLoc == ".java")
        then (compileJava classpath outputDir (outputDir ++ (dropExtension inputLoc) ++ ".java"))
        else putStrLn ""
    )
  putStrLn ""


compileJava :: String -> String -> String -> IO()
compileJava classpath outputDir inputFile = do
  print ("classpath " ++ classpath ++ " outputDir " ++ outputDir ++ " inputFile " ++ inputFile)

  callCommand ("javac -classpath " ++ classpath ++ " -d " ++ outputDir ++ " " ++ inputFile)
