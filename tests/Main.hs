module Main where

import Test.Hspec
import qualified TestCliArguments
import qualified TestDay1 as Day1
import qualified TestDay2 as Day2
import qualified TestDay3 as Day3
import qualified TestDay4 as Day4

main :: IO ()
main = do
  example1 <- readFile "inputs/example1.txt"
  hspec $ do
    TestCliArguments.test
    Day1.test example1
    Day2.test
    Day3.test
    Day4.test
