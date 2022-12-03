module AdventOfCode2022.Day3 where

import Data.Char (ord)
import Data.List
import Data.List.Split (chunksOf)

solve1 :: String -> Int
solve1 xs = sum $ fmap calculatePriority $ lines xs

solve2 :: String -> Int
solve2 xs = sum $ map calculatePriority2 $ chunksOf 3 $ lines xs

calculatePriority :: String -> Int
calculatePriority l = calculateChar . head $ intersect left right
  where
    (left, right) = splitAt (length l `div` 2) l

calculateChar :: Char -> Int
calculateChar c = if oc < oa then oc - oA + 27 else oc - oa + 1
  where
    oc = ord c
    oa = ord 'a'
    oA = ord 'A'

calculatePriority2 :: [String] -> Int
calculatePriority2 l = calculateChar . head $ foldr intersect (head l) l
