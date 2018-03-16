module IOUtils where

import System.FilePath.Posix
import System.Directory

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn dir = getDirectoryContents dir

endsWith :: String -> FilePath -> Bool
endsWith extension file = takeExtension file == extension

cleanDir :: (FilePath -> Bool) -> String -> IO()
cleanDir extensionPredicate directory = do
  allFilesIn directory >>= mapM (\inputLoc ->
    if (endsWith "" inputLoc)
      then cleanDir extensionPredicate (directory ++ inputLoc ++ "/")
      else
        if(extensionPredicate inputLoc)
        then removeFile (directory ++ inputLoc)
        else putStrLn ""
    )
  putStrLn ""
