module TestDay09 where

import qualified AdventOfCode2022.Day09 as D
import Test.Hspec

testInput :: String
testInput =
  "R 4\n\
  \U 4\n\
  \L 3\n\
  \D 1\n\
  \R 4\n\
  \D 1\n\
  \L 5\n\
  \R 2\n\
  \D 2"

testInput2 :: String
testInput2 =
  "R 2\n\
  \D 2\n\
  \U 2\n\
  \D 1\n\
  \L 1\n\
  \D 2\n\
  \R 2\n\
  \L 2\n\
  \R 1\n\
  \D 2"

testInput3 :: String
testInput3 =
  "R 5\n\
  \U 8\n\
  \L 8\n\
  \D 3\n\
  \R 17\n\
  \D 10\n\
  \L 25\n\
  \U 20"

test :: SpecWith ()
test = do
  describe "Day9" $ do
    it "moveTail without change" $ do
      D.moveTail (0, 0) (0, 1) `shouldBe` (0, 0)
      D.moveTail (0, 0) (1, 1) `shouldBe` (0, 0)

    it "moveTail on rows o columns" $ do
      D.moveTail (0, 0) (2, 0) `shouldBe` (1, 0)
      D.moveTail (0, 0) (-2, 0) `shouldBe` (-1, 0)
      D.moveTail (0, 0) (0, 2) `shouldBe` (0, 1)
      D.moveTail (0, 0) (0, -2) `shouldBe` (0, -1)

    it "moveTail diagnoal" $ do
      D.moveTail (0, 0) (2, 1) `shouldBe` (1, 1)
      D.moveTail (0, 0) (2, -1) `shouldBe` (1, -1)
      D.moveTail (0, 0) (-2, 1) `shouldBe` (-1, 1)
      D.moveTail (0, 0) (-2, -1) `shouldBe` (-1, -1)
      D.moveTail (0, 0) (1, 2) `shouldBe` (1, 1)
      D.moveTail (0, 0) (1, -2) `shouldBe` (1, -1)
      D.moveTail (0, 0) (-1, 2) `shouldBe` (-1, 1)
      D.moveTail (0, 0) (-1, -2) `shouldBe` (-1, -1)
      D.moveTail (0, 0) (2, 2) `shouldBe` (1, 1)

    it "solves example (1)" $ do
      D.solve1 testInput `shouldBe` 14
      D.solve1 testInput2 `shouldBe` 6

    it "solves example (2)" $ do
      D.solve2 testInput `shouldBe` 1

    it "solves example (2) big" $ do
      D.solve2 testInput3 `shouldBe` 36
