module AdventOfCode2022.Day11 where

import Data.Bool (bool)
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as M

data Monkey = Monkey
  { monkeyId :: !Int,
    startingItems :: ![Int],
    operation :: !(Int -> Int),
    testDivisibleBy :: !Int,
    ifTrue :: !Int,
    ifFalse :: !Int
  }

type Items = M.Map Int [Int]

type InspectedItems = M.Map Int Int

solve :: Int -> (Int -> Int) -> [Monkey] -> String -> Int
solve n lowerWorryLevel monkeys xs = product . take 2 . reverse . sort . map snd . M.toList . fst $ processMonkeysNTimes lowerWorryLevel n monkeys (startingInspectedItems, startingItems')
  where
    startingItems' = M.fromList $ zip [0 .. length monkeys - 1] $ map startingItems monkeys
    startingInspectedItems = M.fromList $ zip [0 .. length monkeys - 1] (repeat 0)

solve1 :: String -> Int
solve1 xs = solve 20 lowerWorryLevel monkeys xs
  where
    monkeys = parseInput xs
    lowerWorryLevel = floor . (/ 3) . fromIntegral

solve2 :: String -> Int
solve2 xs = solve 10000 lowerWorryLevel monkeys xs
  where
    monkeys = parseInput xs
    w = product $ map testDivisibleBy monkeys
    lowerWorryLevel :: Int -> Int
    lowerWorryLevel = floor . (\i -> fromIntegral $ i `mod` w) . fromIntegral

processMonkeysNTimes :: (Int -> Int) -> Int -> [Monkey] -> (InspectedItems, Items) -> (InspectedItems, Items)
processMonkeysNTimes f 1 ms items = processMonkeys f ms items
processMonkeysNTimes f n ms items = processMonkeysNTimes f (n - 1) ms $ processMonkeys f ms items

processMonkeys :: (Int -> Int) -> [Monkey] -> (InspectedItems, Items) -> (InspectedItems, Items)
processMonkeys _ [] _ = error "Shouldn't happen"
processMonkeys f [m] x = processMonkey f m x
processMonkeys f (m : ms) x = processMonkeys f ms $ processMonkey f m x

processMonkey :: (Int -> Int) -> Monkey -> (InspectedItems, Items) -> (InspectedItems, Items)
processMonkey lowerWorryLevel monkey (inspectedItems, allItems) =
  ( M.adjust (+ length monkeyItems) (monkeyId monkey) inspectedItems,
    foldl throwItems (M.insert (monkeyId monkey) [] allItems) newItems
  )
  where
    monkeyItems = allItems M.! monkeyId monkey
    newItems = map (lowerWorryLevel . operation monkey) monkeyItems

    throwItems :: Items -> Int -> Items
    throwItems items i =
      let newMonkeyId = bool ifFalse ifTrue (i `mod` testDivisibleBy monkey == 0) monkey
       in M.adjust (++ [i]) newMonkeyId items

-- parse input

parseInput :: String -> [Monkey]
parseInput = map parseMonkey . splitOn "\n\n"

parseMonkey :: String -> Monkey
parseMonkey xs = Monkey monkeyId' startingItems' operation' testDivisibleBy' ifTrue' ifFalse'
  where
    l = lines xs
    monkeyId' = read . init . last $ words (l !! 0)
    startingItems' = map read $ splitOn ", " $ last $ splitOn ": " (l !! 1)
    operation' = parseOp . drop 4 $ words (l !! 2)
    testDivisibleBy' = read . last $ words (l !! 3)
    ifTrue' = read . last $ words (l !! 4)
    ifFalse' = read . last $ words (l !! 5)

parseOp :: [String] -> Int -> Int
parseOp ("*" : ["old"]) i = i * i
parseOp ("*" : [n]) i = i * (read n)
parseOp ("+" : [n]) i = i + (read n)
parseOp _ _ = error "unexpected input"
