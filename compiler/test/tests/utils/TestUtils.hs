module TestUtils where

import System.Process


generatedDirectory = "test_classes/"

executeBytecode :: FilePath -> IO String
executeBytecode filePath = do
  results <- readProcess "java" ["-cp", generatedDirectory, filePath] ""
  return results