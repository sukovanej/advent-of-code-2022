module Main (main) where

import qualified AdventOfCode2022.CliArguments as Cli
import qualified AdventOfCode2022.Day01 as Day01
import qualified AdventOfCode2022.Day02 as Day02
import qualified AdventOfCode2022.Day03 as Day03
import qualified AdventOfCode2022.Day04 as Day04
import qualified AdventOfCode2022.Day05 as Day05
import qualified AdventOfCode2022.Day06 as Day06
import qualified AdventOfCode2022.Day07 as Day07
import qualified AdventOfCode2022.Day08 as Day08
import qualified AdventOfCode2022.Day09 as Day09
import qualified AdventOfCode2022.Day10 as Day10
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
getDaySolveFunction 1 1 = Just $ show . Day01.solve1
getDaySolveFunction 1 2 = Just $ show . Day01.solve2
getDaySolveFunction 2 1 = Just $ show . Day02.solve1
getDaySolveFunction 2 2 = Just $ show . Day02.solve2
getDaySolveFunction 3 1 = Just $ show . Day03.solve1
getDaySolveFunction 3 2 = Just $ show . Day03.solve2
getDaySolveFunction 4 1 = Just $ show . Day04.solve1
getDaySolveFunction 4 2 = Just $ show . Day04.solve2
getDaySolveFunction 5 1 = Just Day05.solve1
getDaySolveFunction 5 2 = Just Day05.solve2
getDaySolveFunction 6 1 = Just $ show . Day06.solve1
getDaySolveFunction 6 2 = Just $ show . Day06.solve2
getDaySolveFunction 7 1 = Just $ show . Day07.solve1
getDaySolveFunction 7 2 = Just $ show . Day07.solve2
getDaySolveFunction 8 1 = Just $ show . Day08.solve1
getDaySolveFunction 8 2 = Just $ show . Day08.solve2
getDaySolveFunction 9 1 = Just $ show . Day09.solve1
getDaySolveFunction 9 2 = Just $ show . Day09.solve2
getDaySolveFunction 10 1 = Just $ show . Day10.solve1
getDaySolveFunction 10 2 = Just Day10.solve2
getDaySolveFunction _ _ = Nothing

readInputFile :: String -> IO String
readInputFile filename = readFile $ inputFolder <> "/" <> filename

failWithMessage :: String -> IO a
failWithMessage message = putStrLn message *> exitFailure
