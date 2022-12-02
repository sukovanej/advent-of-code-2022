module TestDay2 where

import qualified AdventOfCode2022.Day2 as Day2
import Test.Hspec

testInput :: String
testInput = "A Y\nB X\nC Z"

test :: SpecWith ()
test = do
  describe "Day2" $ do
    it "solves example (1)" $ do
      Day2.solve1 testInput `shouldBe` 15
    it "solves example (2)" $ do
      Day2.solve2 testInput `shouldBe` 12
