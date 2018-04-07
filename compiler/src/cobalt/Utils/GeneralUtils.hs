{-|
Module      : Utils
Description : Contains various helper functions.
-}
module Utils.GeneralUtils where

import System.FilePath.Posix (takeExtension)
import Data.Char (toLower)

defaultHead :: a -> [a] -> a
defaultHead x [] = x
defaultHead _ xs = head xs

endsWith :: String -> FilePath -> Bool
endsWith extension file = takeExtension file == extension

lowerString :: String -> String
lowerString str = [ toLower loweredString | loweredString <- str]
