module AdventOfCode2022.CliArguments (parseArgs, CliArguments (..)) where

import Control.Applicative
import Data.Foldable (Foldable (foldl'))
import Data.List.Split
import Text.Read

data CliArguments = CliArguments
  { day :: !(Maybe Int),
    inputFileName :: !(Maybe String),
    solution :: !(Maybe Int)
  }
  deriving (Show, Eq)

-- TODO: use lenses instead
dayArguments :: Int -> CliArguments
dayArguments x = CliArguments (Just x) Nothing Nothing

inputFileNameArguments :: String -> CliArguments
inputFileNameArguments x = CliArguments Nothing (Just x) Nothing

solutionArguments :: Int -> CliArguments
solutionArguments x = CliArguments Nothing Nothing (Just x)

instance Semigroup CliArguments where
  a <> b =
    CliArguments
      (day a <|> day b)
      (inputFileName a <|> inputFileName b)
      (solution a <|> solution b)

instance Monoid CliArguments where
  mempty = CliArguments {day = Nothing, inputFileName = Nothing, solution = Nothing}

parseArgs :: [String] -> Either String CliArguments
parseArgs xs = fmap (foldl' (<>) mempty) (traverse parseArgGroup $ chunksOf 2 xs)

parseArgGroup :: [String] -> Either String CliArguments
parseArgGroup ["-f", filename] = Right $ inputFileNameArguments filename
parseArgGroup ["-d", dayStr] = dayArguments <$> parseNumber dayStr
parseArgGroup ["-s", solutionStr] = solutionArguments <$> parseNumber solutionStr
parseArgGroup xs = Left $ "Unexpected arguments " <> unwords xs

parseNumber :: String -> Either String Int
parseNumber input = maybe (Left $ "Expected number, got" <> input) Right $ readMaybe input
