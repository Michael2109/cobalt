import Test.HUnit

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

testBooleanParser :: Test
testBooleanParser = do
  let ast = (parse (booleanParser) "" "True")



  TestCase $ assertEqual "Should correctly parse boolean values"
    (BooleanExpr (BoolConst True))
    (case ast of
      Left  e -> Empty
      Right x -> x)

testSafeHeadForEmptyList :: Test
testSafeHeadForEmptyList =
    TestCase $ assertEqual "Should return Nothing for empty list"
                           Nothing (safeHead ([]::[Int]))

testSafeHeadForNonEmptyList :: Test
testSafeHeadForNonEmptyList =
    TestCase $ assertEqual "Should return (Just head) for non empty list" (Just 1)
               (safeHead ([1]::[Int]))

main :: IO Counts
main = runTestTT $ TestList [
  testBooleanParser,
  testSafeHeadForEmptyList,
  testSafeHeadForNonEmptyList
  ]