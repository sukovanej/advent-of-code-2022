module TestDay06 where

import qualified AdventOfCode2022.Day06 as D
import Test.Hspec

test :: SpecWith ()
test = do
  describe "Day6" $ do
    it "solves example (1)" $ do
      D.solve1 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      D.solve1 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
      D.solve1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      D.solve1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11

    it "solves example (2)" $ do
      D.solve2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19
      D.solve2 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      D.solve2 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23
      D.solve2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29
      D.solve2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
