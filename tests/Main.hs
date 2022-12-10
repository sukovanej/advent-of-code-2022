module Main where

import Test.Hspec
import qualified TestCliArguments
import qualified TestDay1 as Day1
import qualified TestDay2 as Day2
import qualified TestDay3 as Day3
import qualified TestDay4 as Day4
import qualified TestDay5 as Day5
import qualified TestDay6 as Day6
import qualified TestDay7 as Day7
import qualified TestDay8 as Day8
import qualified TestDay9 as Day9
import qualified TestDay10 as Day10

main :: IO ()
main = do
  example1 <- readFile "inputs/example1.txt"
  example10 <- readFile "inputs/example10.txt"

  hspec $ do
    TestCliArguments.test
    Day1.test example1
    Day2.test
    Day3.test
    Day4.test
    Day5.test
    Day6.test
    Day7.test
    Day8.test
    Day9.test
    Day10.test example10
