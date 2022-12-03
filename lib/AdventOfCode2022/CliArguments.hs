{-# LANGUAGE TemplateHaskell #-}

module AdventOfCode2022.CliArguments (parseArgs, CliArguments(CliArguments), day, inputFileName, solution) where

import Control.Applicative
import Control.Lens
import Data.Foldable (Foldable (foldl'))
import Data.List.Split
import Text.Read

data CliArguments = CliArguments
  { _day :: !(Maybe Int),
    _inputFileName :: !(Maybe String),
    _solution :: !(Maybe Int)
  }
  deriving (Show, Eq)

$(makeLenses ''CliArguments)

instance Semigroup CliArguments where
  a <> b =
    CliArguments
      (a ^. day <|> b ^. day)
      (a ^. inputFileName <|> b ^. inputFileName)
      (a ^. solution <|> b ^. solution)

instance Monoid CliArguments where
  mempty = CliArguments {_day = Nothing, _inputFileName = Nothing, _solution = Nothing}

parseArgs :: [String] -> Either String CliArguments
parseArgs xs = fmap (foldl' (<>) mempty) (traverse parseArgGroup $ chunksOf 2 xs)

emptyWith :: ASetter' CliArguments (Maybe a) -> a -> CliArguments
emptyWith l value = l ?~ value $ mempty

parseArgGroup :: [String] -> Either String CliArguments
parseArgGroup ["-f", filename] = emptyWith inputFileName <$> Right filename
parseArgGroup ["-d", dayStr] = emptyWith day <$> parseNumber dayStr
parseArgGroup ["-s", solutionStr] = emptyWith solution <$> parseNumber solutionStr
parseArgGroup xs = Left $ "Unexpected arguments " <> unwords xs

parseNumber :: String -> Either String Int
parseNumber input = maybe (Left $ "Expected number, got" <> input) Right $ readMaybe input
