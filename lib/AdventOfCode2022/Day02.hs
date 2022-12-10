module AdventOfCode2022.Day02 where

data HandShape = Rock | Paper | Scissors deriving (Eq)

instance Ord HandShape where
  compare Rock Scissors = GT
  compare Scissors Paper = GT
  compare Paper Rock = GT
  compare Scissors Rock = LT
  compare Paper Scissors = LT
  compare Rock Paper = LT
  compare _ _ = EQ

data GameEnd = Draw | Win | Loose

solve1 :: String -> Int
solve1 xs =
  sum $
    map (uncurry calculatePoints . mapTuple inputToHandShape) (parse xs)

solve2 :: String -> Int
solve2 xs =
  sum $
    map
      (uncurry calculatePoints2 . bimapTuple inputToHandShape inputToGameEnd)
      (parse xs)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

bimapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimapTuple f g (a, b) = (f a, g b)

calculatePoints :: HandShape -> HandShape -> Int
calculatePoints a b = valueOfResult result + valueOfHandShape b
  where
    result = toResult $ compare a b
    toResult LT = Loose
    toResult GT = Win
    toResult EQ = Draw

calculatePoints2 :: HandShape -> GameEnd -> Int
calculatePoints2 a b = valueOfHandShape (getHandShape a b) + valueOfResult b
  where
    getHandShape x Draw = x
    getHandShape Scissors Win = Rock
    getHandShape Rock Win = Paper
    getHandShape Paper Win = Scissors
    getHandShape Rock Loose = Scissors
    getHandShape Paper Loose = Rock
    getHandShape Scissors Loose = Paper

valueOfResult :: GameEnd -> Int
valueOfResult Draw = 3
valueOfResult Win = 6
valueOfResult Loose = 0

valueOfHandShape :: HandShape -> Int
valueOfHandShape Scissors = 3
valueOfHandShape Paper = 2
valueOfHandShape Rock = 1

parse :: String -> [(Char, Char)]
parse xs = map (\l -> (head l, l !! 2)) $ lines xs

inputToHandShape :: Char -> HandShape
inputToHandShape 'A' = Rock
inputToHandShape 'B' = Paper
inputToHandShape 'C' = Scissors
inputToHandShape 'X' = Rock
inputToHandShape 'Y' = Paper
inputToHandShape 'Z' = Scissors
inputToHandShape _ = error "Invalid input"

inputToGameEnd :: Char -> GameEnd
inputToGameEnd 'X' = Loose
inputToGameEnd 'Y' = Draw
inputToGameEnd 'Z' = Win
inputToGameEnd _ = error "Invalid input"
