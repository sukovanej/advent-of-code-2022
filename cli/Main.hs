module Main (main) where

import qualified AdventOfCode2022.Day1 as Day1
import System.Environment
import CliArguments
import Data.Functor
import System.Exit

inputFolder :: String
inputFolder = "inputs"

main :: IO ()
main = do
  cliArguments <- getArgs <&> parseArgs
  either failWithMessage solveWithArguments cliArguments

solveWithArguments :: CliArguments -> IO ()
solveWithArguments cliArguments =
  maybe (failWithMessage "No day specified") (solveDay cliArguments $ inputFileName cliArguments) (day cliArguments)

solveDay :: CliArguments -> Maybe String -> Int -> IO ()
solveDay cliArguments maybeInputFileName day = do
  input <- readInputFile $ maybe defaultFileName id maybeInputFileName
  maybe (failWithMessage "Solution not specified") (\s -> solveDayWithInput day s input) (solution cliArguments)
    where defaultFileName = "input" <> show day <> ".txt"

solveDayWithInput :: Int -> Int -> String -> IO ()
solveDayWithInput day solution input = do
  let maybeSolve = getDaySolveFunction day solution
  maybe (failWithMessage $ "No solve function for day " <> show day) (\solve -> print $ solve input) maybeSolve

getDaySolveFunction :: Int -> Int -> Maybe (String -> Int)
getDaySolveFunction 1 1 = Just Day1.solve1
getDaySolveFunction 1 2 = Just Day1.solve2
getDaySolveFunction _ _ = Nothing

readInputFile :: String -> IO String
readInputFile filename = readFile $ inputFolder <> "/" <> filename

failWithMessage :: String -> IO ()
failWithMessage message = putStrLn message *> exitFailure
