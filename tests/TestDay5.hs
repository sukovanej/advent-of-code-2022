module TestDay5 where

import qualified AdventOfCode2022.Day5 as Day5
import Test.Hspec

testInput :: String
testInput =
  "    [D]    \n\
  \[N] [C]    \n\
  \[Z] [M] [P]\n\
  \ 1   2   3\n\n\
  \move 1 from 2 to 1\n\
  \move 3 from 1 to 3\n\
  \move 2 from 2 to 1\n\
  \move 1 from 1 to 2\n"

test :: SpecWith ()
test = do
  describe "Day5" $ do
    it "solves example (1)" $ do
      Day5.solve1 testInput `shouldBe` "CMZ"
    it "solves example (2)" $ do
      Day5.solve2 testInput `shouldBe` "MCD"
