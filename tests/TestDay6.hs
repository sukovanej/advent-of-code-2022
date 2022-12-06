module TestDay6 where

import qualified AdventOfCode2022.Day6 as Day6
import Test.Hspec

test :: SpecWith ()
test = do
  describe "Day6" $ do
    it "solves example (1)" $ do
      Day6.solve1 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      Day6.solve1 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
      Day6.solve1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      Day6.solve1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11

    it "solves example (2)" $ do
      Day6.solve2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19
      Day6.solve2 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      Day6.solve2 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23
      Day6.solve2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29
      Day6.solve2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
