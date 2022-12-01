module AdventOfCode2022.Day1 where

import Data.List.Split
import Data.List

solve1 :: String -> Int
solve1 xs = maximum $ map sum $ parseLines $ lines xs

solve2 :: String -> Int
solve2 xs = sum $ take 3 $ reverse $ sort $ map sum $ parseLines $ lines xs

parseLines :: [String] -> [[Int]]
parseLines xs = map (map read) $ splitOn [""] xs
