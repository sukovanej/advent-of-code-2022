module TestDay10 where

import qualified AdventOfCode2022.Day10 as Day10
import Test.Hspec

testInput :: String
testInput = ""

test :: SpecWith ()
test = do
  describe "Day5" $ do
    it "solves example (1)" $ do
      Day10.solve1 testInput `shouldBe` -1
    it "solves example (2)" $ do
      Day10.solve2 testInput `shouldBe` -1
