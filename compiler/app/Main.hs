module Main where

import CompilerExecutor

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  execute args

