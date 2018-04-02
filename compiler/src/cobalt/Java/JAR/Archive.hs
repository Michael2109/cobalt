-- | This module defines functions to read Java JAR files.
module Java.JAR.Archive where

import qualified Data.Map as M
import Codec.Archive.Zip
import Data.Binary
import Data.List
import Data.ByteString.Internal
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import System.FilePath
import Data.Text (unpack)

import Java.ClassPath.Types
import Java.ClassPath.Common
import JVM.ClassFile
import JVM.Converter


readJAREntry :: FilePath -> String -> IO Data.ByteString.Internal.ByteString
readJAREntry jarFile path = do
  s        <- mkEntrySelector jarFile
  content       <- withArchive path (getEntry s)
  return content


-- | Read all entries from JAR file
readAllJAR :: FilePath -> IO [Tree CPEntry]
readAllJAR jarFile = do
    entries <- withArchive jarFile (M.keys <$> getEntries)
    return $ mapF (NotLoadedJAR jarFile) (buildTree $ map unEntrySelector $ filter good entries)
  where
    good file = ".class" `isSuffixOf` (unpack $ getEntryName file)


-- | Read one class from JAR file
readFromJAR :: FilePath -> FilePath -> IO (Class Direct)
readFromJAR jarFile path = do
  s        <- mkEntrySelector jarFile
  content       <- withArchive path (getEntry s)
  return $ classFile2Direct (decode $ fromStrict content)


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

zipJAR :: [Tree (FilePath, Class Direct)] -> ZipArchive ()
zipJAR forest = do
    mapFM go forest
    return ()
  where
    go (path, cls) = do
      s <- mkEntrySelector path
      let src = toStrict $ encodeClass cls
      addEntry Store src s

