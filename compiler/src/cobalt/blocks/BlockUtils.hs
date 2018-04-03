{-|
Module      : BlockUtils
Description : All utilities for the Block modules.
-}
module BlockUtils (
    lowerString
) where

import Data.Char

lowerString str = [ toLower loweredString | loweredString <- str]