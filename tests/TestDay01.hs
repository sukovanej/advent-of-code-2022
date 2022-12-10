module TestDay01 where

import qualified AdventOfCode2022.Day01 as D
import Test.Hspec

test :: String -> SpecWith ()
test exampleInput = do
  describe "Day1" $ do
    it "solves example (1)" $ do
      D.solve1 exampleInput `shouldBe` 24000

    it "solves example (2)" $ do
      D.solve2 exampleInput `shouldBe` 45000
