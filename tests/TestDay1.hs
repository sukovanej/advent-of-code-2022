module TestDay1 where

import qualified AdventOfCode2022.Day1 as Day1
import Test.Hspec

test :: String -> SpecWith ()
test exampleInput = do
  describe "Day1" $ do
    it "solves example (1)" $ do
      Day1.solve1 exampleInput `shouldBe` 24000

    it "solves example (2)" $ do
      Day1.solve2 exampleInput `shouldBe` 45000
