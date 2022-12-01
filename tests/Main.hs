module Main where

import Test.Hspec
import qualified TestDay1 as Day1

main :: IO ()
main = do
  example1 <- readFile $ "inputs/example1.txt"
  hspec $ Day1.test example1
