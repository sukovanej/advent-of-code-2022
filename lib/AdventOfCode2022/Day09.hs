module AdventOfCode2022.Day09 where

import qualified Control.Monad.State.Lazy as S
import qualified Data.Set as Set

data Direction = DLeft | DRight | DUp | DDown deriving (Show)

data Move = Move !Direction !Int

type Position = (Int, Int)

type Rope = (Position, Position)

type VisitedPositions = Set.Set Position

solve1 :: String -> Int
solve1 = Set.size . fst . flip S.execState initialPosition . runMoves . parseInput
  where
    initialPosition = (Set.singleton (0, 0), [(0, 0), (0, 0)])

solve2 :: String -> Int
solve2 = Set.size . fst . flip S.execState initialPosition . runMoves . parseInput
  where
    initialPosition = (Set.singleton (0, 0), replicate 10 (0, 0))

runMoves :: [Move] -> S.State (VisitedPositions, [Position]) ()
runMoves = foldr ((>>) . runMove) (return ())

runMove :: Move -> S.State (VisitedPositions, [Position]) ()
runMove (Move _ 0) = return ()
runMove (Move direction n) = S.modify (modify direction) >> runMove (Move direction (n - 1))

modify :: Direction -> (VisitedPositions, [Position]) -> (VisitedPositions, [Position])
modify direction (visited, rope) = (Set.insert (last newRope) visited, newRope)
  where
    newRope = moveRope (moveHead direction $ head rope) (tail rope)

moveRope :: Position -> [Position] -> [Position]
moveRope x [] = [x]
moveRope newHead (tail' : xs) = newHead : moveRope newTail xs
  where
    newTail = moveTail tail' newHead

moveHead :: Direction -> Position -> Position
moveHead DRight (x, y) = (x + 1, y)
moveHead DLeft (x, y) = (x - 1, y)
moveHead DUp (x, y) = (x, y + 1)
moveHead DDown (x, y) = (x, y - 1)

moveTail :: Position -> Position -> Position
moveTail (tx, ty) (hx, hy)
  | abs (tx - hx) == 0 && abs (ty - hy) == 2 = (hx, hy + signum (ty - hy))
  | abs (tx - hx) == 2 && abs (ty - hy) == 0 = (hx + signum (tx - hx), hy)
  | abs (tx - hx) == 2 && abs (ty - hy) == 1 = (hx + signum (tx - hx), hy)
  | abs (tx - hx) == 1 && abs (ty - hy) == 2 = (hx, hy + signum (ty - hy))
  | abs (tx - hx) == 2 && abs (ty - hy) == 2 = (hx + signum (tx - hx), hy + signum (ty - hy))
  | otherwise = (tx, ty)

-- parsing

parseInput :: String -> [Move]
parseInput = map parse' . lines
  where
    parse' ('R' : ' ' : xs) = Move DRight (read xs)
    parse' ('L' : ' ' : xs) = Move DLeft (read xs)
    parse' ('U' : ' ' : xs) = Move DUp (read xs)
    parse' ('D' : ' ' : xs) = Move DDown (read xs)
    parse' _ = error "Shouldn't happen"
