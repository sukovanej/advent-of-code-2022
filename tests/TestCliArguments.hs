module TestCliArguments where

import qualified AdventOfCode2022.CliArguments as Cli
import Test.Hspec

test :: SpecWith ()
test = do
  describe "CliArguments" $ do
    it "returns empty" $ do
      Cli.parseArgs [] `shouldBe` Right (Cli.CliArguments Nothing Nothing Nothing)

    it "returns correct arguments for single input" $ do
      Cli.parseArgs ["-d", "1"] `shouldBe` Right (Cli.CliArguments (Just 1) Nothing Nothing)
      Cli.parseArgs ["-s", "1"] `shouldBe` Right (Cli.CliArguments Nothing Nothing (Just 1))
      Cli.parseArgs ["-f", "example.txt"] `shouldBe` Right (Cli.CliArguments Nothing (Just "example.txt") Nothing)

    it "returns correct arguments for two inputs" $ do
      Cli.parseArgs ["-d", "1", "-s", "2"] `shouldBe` Right (Cli.CliArguments (Just 1) Nothing (Just 2))
      Cli.parseArgs ["-s", "2", "-d", "1"] `shouldBe` Right (Cli.CliArguments (Just 1) Nothing (Just 2))

      Cli.parseArgs ["-d", "1", "-f", "example.txt"] `shouldBe` Right (Cli.CliArguments (Just 1) (Just "example.txt") Nothing)
      Cli.parseArgs ["-f", "example.txt", "-d", "1"] `shouldBe` Right (Cli.CliArguments (Just 1) (Just "example.txt") Nothing)

    it "handles correctly unexpected arguments" $ do
      Cli.parseArgs ["-d"] `shouldBe` Left "Unexpected arguments -d"
      Cli.parseArgs ["-d", "1", "-d"] `shouldBe` Left "Unexpected arguments -d"
      Cli.parseArgs ["-unknown", "1"] `shouldBe` Left "Unexpected arguments -unknown 1"
