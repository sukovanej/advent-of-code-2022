module TestDay02 where

import qualified AdventOfCode2022.Day02 as D
import Test.Hspec

testInput :: String
testInput = "A Y\nB X\nC Z"

test :: SpecWith ()
test = do
  describe "Day2" $ do
    it "solves example (1)" $ do
      D.solve1 testInput `shouldBe` 15
    it "solves example (2)" $ do
      D.solve2 testInput `shouldBe` 12
