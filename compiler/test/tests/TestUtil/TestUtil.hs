module TestUtil.TestUtil where

import System.Process

inputDirectory :: String
inputDirectory = "test/resources/"

outputDirectory :: String
outputDirectory = "cobalt_generated_classes/"

executeBytecode :: FilePath -> IO String
executeBytecode filePath = do
  results <- readProcess "java" ["-cp", outputDirectory, filePath] ""
  return results
