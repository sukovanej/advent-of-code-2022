module TestDay10 where

import qualified AdventOfCode2022.Day10 as Day10
import Test.Hspec

testInput :: String
testInput = ""

expectedDrawing :: String
expectedDrawing =
  "##..##..##..##..##..##..##..##..##..##..\n\
  \###...###...###...###...###...###...###.\n\
  \####....####....####....####....####....\n\
  \#####.....#####.....#####.....#####.....\n\
  \######......######......######......####\n\
  \#######.......#######.......#######.....\n"

test :: String -> SpecWith ()
test exampleInput = do
  describe "Day10" $ do
    it "solves example (1)" $ do
      Day10.solve1 exampleInput `shouldBe` 13140

    it "solves example (2)" $ do
      Day10.solve2 exampleInput `shouldBe` expectedDrawing
