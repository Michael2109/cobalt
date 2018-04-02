-- | This module defines functions to read Java JAR files.
module Java.JAR.Archive where

import Codec.Archive.Zip
import Data.Binary
import Data.List
import Data.ByteString.Lazy
import System.FilePath

import Java.ClassPath.Types
import Java.ClassPath.Common
import JVM.ClassFile
import JVM.Converter

{--
readJAREntry :: (Enum a) => FilePath -> String -> IO (Maybe [a])
readJAREntry jarfile path = do
  s        <- mkEntrySelector jarFile
  content       <- withArchive path (getEntry s)
  catchZipError (Just `fmap` (content))
                    (\_ -> return Nothing)

-- | Read all entires from JAR file
readAllJAR :: FilePath -> IO [Tree CPEntry]
readAllJAR jarfile = do
    files <- Zip.withArchive [] jarfile $ Zip.fileNames []
    return $ mapF (NotLoadedJAR jarfile) (buildTree $ filter good files)
  where
    good file = ".class" `isSuffixOf` file
--}
-- | Read one class from JAR file
{--
readFromJAR :: FilePath -> FilePath -> IO (Class Direct)
readFromJAR jarFile path = do
  s        <- mkEntrySelector jarFile
  content       <- withArchive path (getEntry s)
  return $ classFile2Direct (decode $ toLazy content)
--}
{--
checkClassTree :: [Tree CPEntry] -> IO [Tree (FilePath, Class Direct)]
checkClassTree forest = mapFMF check forest
  where
    check _ (NotLoaded path) = do
       cls <- parseClassFile path
       return (path, cls)
    check a (Loaded path cls) = return (a </> path, cls)
    check a (NotLoadedJAR jar path) = do
       cls <- readFromJAR jar (a </> path)
       return (a </> path, cls)
    check a (LoadedJAR _ cls) =
       return (a </> show (thisClass cls), cls)
--}
zipJAR :: [Tree (FilePath, Class Direct)] -> ZipArchive ()
zipJAR forest = do
    mapFM go forest
    return ()
  where
    go (path, cls) = do
      s <- mkEntrySelector path
      let src = toStrict $ encodeClass cls
      addEntry Store src s

