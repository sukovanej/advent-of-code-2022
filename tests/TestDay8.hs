module TestDay8 where

import qualified AdventOfCode2022.Day8 as Day8
import Test.Hspec

testInput :: String
testInput =
  "30373\n\
  \25512\n\
  \65332\n\
  \33549\n\
  \35390"

grid :: Day8.Grid
grid = Day8.parse testInput

test :: SpecWith ()
test = do
  describe "Day8" $ do
    it "visible in row" $ do
      Day8.visible "1132145" `shouldBe` [0, 2, 5, 6]
      Day8.findVisibleInRow  "1132145" `shouldBe` [0, 2, 5, 6, 6]
      Day8.findVisibleInRow  "113214542" `shouldBe` [0, 2, 5, 6, 6, 7, 8]
      Day8.findVisibleInRow "30373" `shouldBe` [0, 3, 3, 4]

    it "calculate view (1)" $ do
      Day8.viewLeft grid (1, 2) `shouldBe` 1
      Day8.viewRight grid (1, 2) `shouldBe` 2
      Day8.viewUp grid (1, 2) `shouldBe` 1
      Day8.viewDown grid (1, 2) `shouldBe` 2

      Day8.calculateView grid (1, 2) `shouldBe` 4

    it "calculate view (2)" $ do
      Day8.viewLeft grid (3, 2) `shouldBe` 2
      Day8.viewRight grid (3, 2) `shouldBe` 2
      Day8.viewUp grid (3, 2) `shouldBe` 2
      Day8.viewDown grid (3, 2) `shouldBe` 1

      Day8.calculateView grid (3, 2) `shouldBe` 8

    it "solves example (1)" $ do
      Day8.solve1 testInput `shouldBe` 21

    it "solves example (2)" $ do
      Day8.solve2 testInput `shouldBe` 8
