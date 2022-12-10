module TestDay07 where

import AdventOfCode2022.Day07
import Test.Hspec

testInput :: String
testInput =
  "$ cd /\n\
  \$ ls\n\
  \dir a\n\
  \14848514 b.txt\n\
  \8504156 c.dat\n\
  \dir d\n\
  \$ cd a\n\
  \$ ls\n\
  \dir e\n\
  \29116 f\n\
  \2557 g\n\
  \62596 h.lst\n\
  \$ cd e\n\
  \$ ls\n\
  \584 i\n\
  \$ cd ..\n\
  \$ cd ..\n\
  \$ cd d\n\
  \$ ls\n\
  \4060174 j\n\
  \8033020 d.log\n\
  \5626152 d.ext\n\
  \7214296 k\n"

test :: SpecWith ()
test = do
  describe "Day7" $ do
    it "resulToTree (1)" $ do
      resultToTree [Cd "/", Ls [LsFile 1 "1", LsDir "a"]]
        `shouldBe` Dir () "/" [File 1 "1", Dir () "a" []]

      resultToTree [Cd "/", Ls [LsDir "a"], Cd "a", Ls [LsFile 1 "1"]]
        `shouldBe` Dir () "/" [Dir () "a" [File 1 "1"]]

      resultToTree [Cd "/", Ls [LsDir "a", LsDir "b"], Cd "a", Ls [LsFile 1 "1"], Cd "..", Cd "b", Ls [LsFile 2 "2"]]
        `shouldBe` Dir () "/" [Dir () "a" [File 1 "1"], Dir () "b" [File 2 "2"]]

      resultToTree
        [ Cd "/",
          Ls [LsDir "a", LsDir "b", LsDir "c"],
          Cd "a",
          Ls [LsDir "c"],
          Cd "c",
          Ls [LsFile 1 "1"],
          Cd "..",
          Cd "..",
          Cd "b",
          Ls [LsDir "e", LsFile 2 "2"],
          Cd "e",
          Ls [LsFile 3 "3"],
          Cd "..",
          Cd "..",
          Cd "c",
          Ls [LsFile 4 "4"]
        ]
        `shouldBe` Dir
          ()
          "/"
          [ Dir () "a" [Dir () "c" [File 1 "1"]],
            Dir () "b" [File 2 "2", Dir () "e" [File 3 "3"]],
            Dir () "c" [File 4 "4"]
          ]

    it "solves example (1)" $ do
      solve1 testInput `shouldBe` 95437

    it "solves example (2)" $ do
      solve2 testInput `shouldBe` 24933642
