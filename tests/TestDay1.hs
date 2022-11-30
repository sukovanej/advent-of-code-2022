module TestDay1 where

import Test.Hspec

import qualified AdventOfCode2022.Day1 as Day1

test :: SpecWith ()
test = do
  describe "Day1" $ do
    describe "solve" $ do
      it "dummy" $ do
        Day1.solve `shouldBe` 1
