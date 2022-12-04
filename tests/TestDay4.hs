module TestDay4 where

import qualified AdventOfCode2022.Day4 as Day4
import Test.Hspec

testInput :: String
testInput = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

test :: SpecWith ()
test = do
  describe "Day4" $ do
    it "solves example (1)" $ do
      Day4.solve1 testInput `shouldBe` 2
    it "solves example (2)" $ do
      Day4.solve2 testInput `shouldBe` 4
