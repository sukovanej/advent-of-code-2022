module Main where

import Test.Hspec
import qualified TestCliArguments
import qualified TestDay01 as Day01
import qualified TestDay02 as Day02
import qualified TestDay03 as Day03
import qualified TestDay04 as Day04
import qualified TestDay05 as Day05
import qualified TestDay06 as Day06
import qualified TestDay07 as Day07
import qualified TestDay08 as Day08
import qualified TestDay09 as Day09
import qualified TestDay10 as Day10
import qualified TestDay11 as Day11

main :: IO ()
main = do
  example1 <- readFile "inputs/example1.txt"
  example10 <- readFile "inputs/example10.txt"

  hspec $ do
    TestCliArguments.test
    Day01.test example1
    Day02.test
    Day03.test
    Day04.test
    Day05.test
    Day06.test
    Day07.test
    Day08.test
    Day09.test
    Day10.test example10
    Day11.test
