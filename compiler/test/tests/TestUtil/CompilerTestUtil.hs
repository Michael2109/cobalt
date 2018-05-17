module TestUtil.CompilerTestUtil where

import System.Directory
import System.FilePath.Posix
import System.Environment (withArgs)

import Compiler.CompilerExecutor

allFilesIn dir = getDirectoryContents dir

compileDirectory :: FilePath -> FilePath -> IO ()
compileDirectory inputDir outputDir = do
    allFilesIn inputDir >>= mapM (\inputLoc ->
        if (takeExtension inputLoc == "")
        then compileDirectory (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
        else
            if(takeExtension inputLoc == ".cobalt")
            then withArgs ["-d", outputDir, "-p", "test/resources/integration/", inputLoc] execute
            else putStrLn ""
        )

    putStrLn "Complete"
