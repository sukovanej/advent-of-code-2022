module TestDay03 where

import qualified AdventOfCode2022.Day03 as D
import Test.Hspec

testInput :: String
testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

test :: SpecWith ()
test = do
  describe "Day3" $ do
    it "solves example (1)" $ do
      D.solve1 testInput `shouldBe` 157
    it "solves example (2)" $ do
      D.solve2 testInput `shouldBe` 70
