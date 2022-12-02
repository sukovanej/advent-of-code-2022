module Main where

import Test.Hspec
import qualified TestDay1 as Day1
import qualified TestDay2 as Day2

main :: IO ()
main = do
  example1 <- readFile "inputs/example1.txt"
  hspec $ do
    Day1.test example1
    Day2.test
