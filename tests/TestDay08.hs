module TestDay08 where

import qualified AdventOfCode2022.Day08 as D
import Test.Hspec

testInput :: String
testInput =
  "30373\n\
  \25512\n\
  \65332\n\
  \33549\n\
  \35390"

grid :: D.Grid
grid = D.parse testInput

test :: SpecWith ()
test = do
  describe "Day8" $ do
    it "visible in row" $ do
      D.visible "1132145" `shouldBe` [0, 2, 5, 6]
      D.findVisibleInRow  "1132145" `shouldBe` [0, 2, 5, 6, 6]
      D.findVisibleInRow  "113214542" `shouldBe` [0, 2, 5, 6, 6, 7, 8]
      D.findVisibleInRow "30373" `shouldBe` [0, 3, 3, 4]

    it "calculate view (1)" $ do
      D.viewLeft grid (1, 2) `shouldBe` 1
      D.viewRight grid (1, 2) `shouldBe` 2
      D.viewUp grid (1, 2) `shouldBe` 1
      D.viewDown grid (1, 2) `shouldBe` 2

      D.calculateView grid (1, 2) `shouldBe` 4

    it "calculate view (2)" $ do
      D.viewLeft grid (3, 2) `shouldBe` 2
      D.viewRight grid (3, 2) `shouldBe` 2
      D.viewUp grid (3, 2) `shouldBe` 2
      D.viewDown grid (3, 2) `shouldBe` 1

      D.calculateView grid (3, 2) `shouldBe` 8

    it "solves example (1)" $ do
      D.solve1 testInput `shouldBe` 21

    it "solves example (2)" $ do
      D.solve2 testInput `shouldBe` 8
