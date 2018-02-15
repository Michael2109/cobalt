module BlockUtils (
    lowerString
) where

import Data.Char

lowerString str = [ toLower loweredString | loweredString <- str]