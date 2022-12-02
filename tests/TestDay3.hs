module TestDay3 where

import qualified AdventOfCode2022.Day3 as Day3
import Test.Hspec

testInput :: String
testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

test :: SpecWith ()
test = do
  describe "Day3" $ do
    it "solves example (1)" $ do
      Day3.solve1 testInput `shouldBe` 157
    it "solves example (2)" $ do
      Day3.solve2 testInput `shouldBe` 70
