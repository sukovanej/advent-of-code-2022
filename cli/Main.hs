module Main (main) where

import qualified AdventOfCode2022.CliArguments as Cli
import qualified AdventOfCode2022.Day1 as Day1
import qualified AdventOfCode2022.Day10 as Day10
import qualified AdventOfCode2022.Day2 as Day2
import qualified AdventOfCode2022.Day3 as Day3
import qualified AdventOfCode2022.Day4 as Day4
import qualified AdventOfCode2022.Day5 as Day5
import qualified AdventOfCode2022.Day6 as Day6
import qualified AdventOfCode2022.Day7 as Day7
import qualified AdventOfCode2022.Day8 as Day8
import qualified AdventOfCode2022.Day9 as Day9
import Control.Lens
import System.Environment
import System.Exit

-- read more on strict
data Inputs = Inputs
  { inputFileName :: !String,
    solveFn :: !(String -> String)
  }

inputFolder :: String
inputFolder = "inputs"

main :: IO ()
main = do
  maybeCliArguments <- (getArgs <&> Cli.parseArgs) <&> (>>= parseCli)
  cliArguments <- either failWithMessage return maybeCliArguments
  input <- readInputFile $ inputFileName cliArguments
  putStrLn $ solveFn cliArguments input

parseCli :: Cli.CliArguments -> Either String Inputs
parseCli args = do
  dayArg <- maybe (Left "Day not specified") Right (args ^. Cli.day)
  solutionArg <- maybe (Left "Solution not specified") Right (args ^. Cli.solution)
  inputFileNameArg <- maybe (Right $ "input" <> show dayArg <> ".txt") Right (args ^. Cli.inputFileName)
  solve <- maybe (Left "Solve function not set") Right (getDaySolveFunction dayArg solutionArg)
  return $ Inputs inputFileNameArg solve

getDaySolveFunction :: Int -> Int -> Maybe (String -> String)
getDaySolveFunction 1 1 = Just $ show . Day1.solve1
getDaySolveFunction 1 2 = Just $ show . Day1.solve2
getDaySolveFunction 2 1 = Just $ show . Day2.solve1
getDaySolveFunction 2 2 = Just $ show . Day2.solve2
getDaySolveFunction 3 1 = Just $ show . Day3.solve1
getDaySolveFunction 3 2 = Just $ show . Day3.solve2
getDaySolveFunction 4 1 = Just $ show . Day4.solve1
getDaySolveFunction 4 2 = Just $ show . Day4.solve2
getDaySolveFunction 5 1 = Just Day5.solve1
getDaySolveFunction 5 2 = Just Day5.solve2
getDaySolveFunction 6 1 = Just $ show . Day6.solve1
getDaySolveFunction 6 2 = Just $ show . Day6.solve2
getDaySolveFunction 7 1 = Just $ show . Day7.solve1
getDaySolveFunction 7 2 = Just $ show . Day7.solve2
getDaySolveFunction 8 1 = Just $ show . Day8.solve1
getDaySolveFunction 8 2 = Just $ show . Day8.solve2
getDaySolveFunction 9 1 = Just $ show . Day9.solve1
getDaySolveFunction 9 2 = Just $ show . Day9.solve2
getDaySolveFunction 10 1 = Just $ show . Day10.solve1
getDaySolveFunction 10 2 = Just Day10.solve2
getDaySolveFunction _ _ = Nothing

readInputFile :: String -> IO String
readInputFile filename = readFile $ inputFolder <> "/" <> filename

failWithMessage :: String -> IO a
failWithMessage message = putStrLn message *> exitFailure
