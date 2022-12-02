module Main (main) where

import qualified AdventOfCode2022.Day1 as Day1
import qualified AdventOfCode2022.Day2 as Day2
import qualified AdventOfCode2022.Day3 as Day3
import qualified CliArguments as Cli
import Data.Functor
import System.Environment
import System.Exit

-- read more on strict
data Inputs = Inputs
  { inputFileName :: !String,
    solveFn :: !(String -> Int)
  }

inputFolder :: String
inputFolder = "inputs"

main :: IO ()
main = do
  maybeCliArguments <- (getArgs <&> Cli.parseArgs) <&> (>>= parseCli)
  cliArguments <- either failWithMessage return maybeCliArguments
  input <- readInputFile $ inputFileName cliArguments
  print $ solveFn cliArguments input

parseCli :: Cli.CliArguments -> Either String Inputs
parseCli args = do
  dayArg <- maybe (Left "Day not specified") Right (Cli.day args)
  solutionArg <- maybe (Left "Solution not specified") Right (Cli.solution args)
  inputFileNameArg <- maybe (Right $ "input" <> show dayArg <> ".txt") Right (Cli.inputFileName args)
  solve <- maybe (Left "Solve function not set") Right (getDaySolveFunction dayArg solutionArg)
  return $ Inputs inputFileNameArg solve

getDaySolveFunction :: Int -> Int -> Maybe (String -> Int)
getDaySolveFunction 1 1 = Just Day1.solve1
getDaySolveFunction 1 2 = Just Day1.solve2
getDaySolveFunction 2 1 = Just Day2.solve1
getDaySolveFunction 2 2 = Just Day2.solve2
getDaySolveFunction 3 1 = Just Day3.solve1
getDaySolveFunction 3 2 = Just Day3.solve2
getDaySolveFunction _ _ = Nothing

readInputFile :: String -> IO String
readInputFile filename = readFile $ inputFolder <> "/" <> filename

failWithMessage :: String -> IO a
failWithMessage message = putStrLn message *> exitFailure
