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
  maybe (failWithMessage "No day specified") (solveDay $ inputFileName cliArguments) (day cliArguments)

solveDay :: Maybe String -> Int -> IO ()
solveDay maybeInputFileName day = do
  input <- readInputFile $ maybe defaultFileName id maybeInputFileName
  solveDayWithInput day input
    where defaultFileName = "input" <> show day <> ".txt"

solveDayWithInput :: Int -> String -> IO ()
solveDayWithInput day input = do
  let maybeSolve = getDaySolveFunction day
  maybe (failWithMessage $ "No solve function for day " <> show day) (\solve -> print $ solve input) maybeSolve

getDaySolveFunction :: Int -> Maybe (String -> Int)
getDaySolveFunction 1 = Just Day1.solve
getDaySolveFunction _ = Nothing

readInputFile :: String -> IO String
readInputFile filename = readFile $ inputFolder <> "/" <> filename

failWithMessage :: String -> IO ()
failWithMessage message = putStrLn message *> exitFailure
