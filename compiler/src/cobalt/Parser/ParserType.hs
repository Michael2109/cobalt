{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Parser.ParserType where

import qualified Data.Set as E
import Text.Megaparsec
import Data.Void
import Data.Set (Set)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Data
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set             as S

data ErrorWrapper = ErrorWrapper
    (Maybe (ErrorItem Char)) -- unexpected part
    (Set (ErrorItem Char))   -- expected part
    String                   -- custom data, e.g. a message
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ErrorWrapper where
  showErrorComponent (ErrorWrapper us es msg) =
    parseErrorTextPretty (TrivialError undefined us es :: ParseError Char Void)
    -- parseErrorTextPretty does not look at position
    ++ msg

type Parser = Parsec ErrorWrapper String

addContext :: String -> Parser a -> Parser a
addContext msg = region f
  where
    f (TrivialError pos us es) =
      FancyError pos (S.singleton $ ErrorCustom (ErrorWrapper us es msg))
    f err = err -- other options are possible, but this is an easy one
