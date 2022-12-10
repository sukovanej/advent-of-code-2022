module AdventOfCode2022.Day10 where

import qualified Control.Monad.State.Lazy as S
import Data.Bool (bool)
import Data.List.Split (chunksOf)

data Instr = Noop | Addx !Int

type Cycle = Int

type Acc = Int

type Value = Int

type Crt = [Bool]

solve1 :: String -> Int
solve1 = trd . flip S.execState (1, 1, 0) . runInstrs updateAcc . parseInput

solve2 :: String -> String
solve2 = unlines . chunksOf 40 . map (bool '.' '#') . trd . flip S.execState (1, 1, []) . runInstrs updateCrt . parseInput

runInstrs :: (Cycle -> Value -> a -> a) -> [Instr] -> S.State (Cycle, Value, a) ()
runInstrs f = foldr ((>>) . S.modify . runInstr f) (return ())

runInstr :: (Cycle -> Value -> a -> a) -> Instr -> (Cycle, Value, a) -> (Cycle, Value, a)
runInstr f Noop (c, value, a) = (c + 1, value, f c value a)
runInstr f (Addx x) (c, value, a) = (c + 2, value + x, f (c + 1) value . f c value $ a)

updateAcc :: Cycle -> Value -> Acc -> Acc
updateAcc c value acc = if c `elem` [20, 60, 100, 140, 180, 220] then acc + c * value else acc

updateCrt :: Cycle -> Value -> Crt -> Crt
updateCrt _ value crt = if (length crt `mod` 40) `elem` [value - 1, value, value + 1] then crt ++ [True] else crt ++ [False]

trd :: (a, b, c) -> c
trd (_, _, x) = x

parseInput :: String -> [Instr]
parseInput = map parseLine . lines
  where
    parseLine ('n' : 'o' : 'o' : "p") = Noop
    parseLine ('a' : 'd' : 'd' : 'x' : ' ' : xs) = Addx (read xs)
    parseLine _ = Noop
