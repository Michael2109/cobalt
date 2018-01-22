module Lib
    ( someFunc,
    solution,
    doubleMe,
    add,
    doubleSmallNumber,
    say
    ) where

import Data.List
import System.IO

-- Int
minInt = minBound:: Int
maxInt = maxBound:: Int

someFunc :: IO ()
someFunc = print minInt


solution :: Int -> Int -> Int
solution a b  = do
    a * b

doubleMe :: [Int] -> [Int]
doubleMe list = do
    map (* 2) list

add :: Int -> Int -> Int
add x y = do
    x + y

doubleSmallNumber x = if(x > 100)
                        then x
                        else x * 2

say :: (Integral a) => a -> String
say 1 = "One!"
say 2 = "Two!"
say 3 = "Three!"
say x = "Higher than 3!"