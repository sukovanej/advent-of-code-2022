module Main where

import Test.Hspec
import qualified TestDay1 as Day1

main :: IO ()
main = hspec $ Day1.test
