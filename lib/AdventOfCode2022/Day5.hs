{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode2022.Day5 where

import AdventOfCode2022.Parsing
import Data.Attoparsec.Text hiding (takeWhile)
import Data.List (transpose)
import Data.Maybe (catMaybes)

type Stack = [Char]

type StackList = [Stack]

type Move = (Int, Int, Int)

solve1 :: String -> String
solve1 xs = map head $ uncurry (applyMoves reverse) $ parseInput xs

solve2 :: String -> String
solve2 xs = map head $ uncurry (applyMoves id) $ parseInput xs

applyMoves :: (Stack -> Stack) -> StackList -> [Move] -> StackList
applyMoves _ s [] = s
applyMoves t s ((move, from, to) : ms) = applyMoves t newStackList ms
  where
    newStackList = replaceAt (from - 1) newFromStackList . replaceAt (to - 1) newToStackList $ s
    (toBeMoved, newFromStackList) = splitAt move $ s !! (from - 1)
    newToStackList = t toBeMoved ++ (s !! (to - 1))

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index value = zipWith (\i v -> if i == index then value else v) [0 ..]

parseInput :: String -> (StackList, [Move])
parseInput input = (inputStackList, moves)
  where
    inputLines = lines input
    moves = map parseMove $ tail $ dropWhile (/= "") inputLines
    parseMove line = unsafeParse moveExpr line
    moveExpr = (,,) <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal)

    charExpr = choice [Just <$> (char '[' *> anyChar <* char ']'), Nothing <$ string "   "]
    stackLineExpr = sepBy charExpr (char ' ')
    inputStackList = map catMaybes $ transpose $ map (unsafeParse stackLineExpr) $ (init . takeWhile (/= "")) inputLines
