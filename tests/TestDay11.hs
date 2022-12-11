module TestDay11 where

import qualified AdventOfCode2022.Day11 as Day11
import Test.Hspec

exampleInput :: String
exampleInput =
  "Monkey 0:\n\
  \  Starting items: 79, 98\n\
  \  Operation: new = old * 19\n\
  \  Test: divisible by 23\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 3\n\
  \\n\
  \Monkey 1:\n\
  \  Starting items: 54, 65, 75, 74\n\
  \  Operation: new = old + 6\n\
  \  Test: divisible by 19\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 0\n\
  \\n\
  \Monkey 2:\n\
  \  Starting items: 79, 60, 97\n\
  \  Operation: new = old * old\n\
  \  Test: divisible by 13\n\
  \    If true: throw to monkey 1\n\
  \    If false: throw to monkey 3\n\
  \\n\
  \Monkey 3:\n\
  \  Starting items: 74\n\
  \  Operation: new = old + 3\n\
  \  Test: divisible by 17\n\
  \    If true: throw to monkey 0\n\
  \    If false: throw to monkey 1\n"

test :: SpecWith ()
test = do
  describe "Day11" $ do
    it "solves example (1)" $ do
      Day11.solve1 exampleInput `shouldBe` 10605

    it "solves example (2)" $ do
      Day11.solve2 exampleInput `shouldBe` 2713310158
