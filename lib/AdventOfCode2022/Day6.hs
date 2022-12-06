module AdventOfCode2022.Day6 where

import Data.List

solve1 :: String -> Int
solve1 = solve 4

solve2 :: String -> Int
solve2 = solve 14

solve :: Int -> String -> Int
solve n xs = if (length . nub . take n) xs == n then n else 1 + solve n (tail xs)
