module AdventOfCode2022.Day3 where

import qualified Data.Set as Set
import Data.Char (ord)
import Data.List.Split (chunksOf)

solve1 :: String -> Int
solve1 xs = sum $ fmap calculatePriority $ lines xs

calculatePriority :: String -> Int
calculatePriority l = calculateChar . head . Set.toList $ Set.intersection (Set.fromList left) (Set.fromList right)
  where (left, right) = splitAt (length l `div` 2) l

calculateChar :: Char -> Int
calculateChar c = if oc < oa then oc - oA + 27 else oc - oa + 1
  where oc = ord c
        oa = ord 'a'
        oA = ord 'A'

solve2 :: String -> Int
solve2 xs = sum $ map calculatePriority2 $ chunksOf 3 $ lines xs

calculatePriority2 :: [String] -> Int
calculatePriority2 l = calculateChar . head . Set.toList $ foldr Set.intersection (Set.fromList . head $ l) $ sets
  where sets = map Set.fromList l
