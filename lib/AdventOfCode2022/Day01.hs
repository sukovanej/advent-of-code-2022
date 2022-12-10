module AdventOfCode2022.Day01 where

import Data.List
import Data.List.Split

solve1 :: String -> Int
solve1 xs = maximum $ map sum $ parseLines $ lines xs

solve2 :: String -> Int
solve2 xs = sum $ take 3 $ reverse $ sort $ map sum $ parseLines $ lines xs

parseLines :: [String] -> [[Int]]
parseLines xs = map (map read) $ splitOn [""] xs
