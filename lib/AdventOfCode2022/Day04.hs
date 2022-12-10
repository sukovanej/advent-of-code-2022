module AdventOfCode2022.Day04 where

import AdventOfCode2022.Parsing
import Data.Attoparsec.Text

solve1 :: String -> Int
solve1 xs = length $ filter containsTheOther $ parseInput xs

solve2 :: String -> Int
solve2 xs = length $ filter overlap $ parseInput xs

containsTheOther :: ((Int, Int), (Int, Int)) -> Bool
containsTheOther ((a, b), (x, y)) = a <= x && b >= y || x <= a && y >= b

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (x, y)) = not (b < x || a > y)

parseInput :: String -> [((Int, Int), (Int, Int))]
parseInput = unsafeParse parseLines
  where
    parseLines = lineExpr `sepBy` endOfLine <* skipMany endOfLine
    lineExpr = (,) <$> numberTupleExpr <* char ',' <*> numberTupleExpr
    numberTupleExpr = (,) <$> decimal <* char '-' <*> decimal
