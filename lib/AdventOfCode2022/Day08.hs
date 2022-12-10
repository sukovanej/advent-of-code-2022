{-# LANGUAGE TupleSections #-}

module AdventOfCode2022.Day08 where

import Data.List (nub, transpose)

type Row = [Char]

type Grid = [Row]

type Views = [[Int]]

type Position = (Int, Int)

solve1 :: String -> Int
solve1 = findVisible . parse

-- solution 1

findVisible :: Grid -> Int
findVisible grid = length . nub $ inRows ++ inColumns
  where
    inRows = concat $ zipWith (\index row -> map (index,) (findVisibleInRow row)) [0 ..] grid
    inColumns = concat $ zipWith (\index column -> map (,index) (findVisibleInRow column)) [0 ..] (transpose grid)

findVisibleInRow :: Row -> [Int]
findVisibleInRow row = visible row ++ (reverse . map (length row - 1 -) . visible $ reverse row)

visible :: Row -> [Int]
visible xs = 0 : visible' (zip [0 ..] xs)
  where
    visible' [] = error "shouldn't hapen"
    visible' [_] = []
    visible' (hh@(_, h) : xx@(xi, x) : rs) =
      if x > h
        then xi : visible' (xx : rs)
        else visible' (hh : rs)

-- solution 2

solve2 :: String -> Int
solve2 = maximum . calculateViews . parse

calculateViews :: Grid -> [Int]
calculateViews xs = map (calculateView xs) allPositions
  where
    allPositions :: [Position]
    allPositions = (,) <$> [1 .. (length xs - 2)] <*> [1 .. ((length . head) xs - 2)]

calculateView :: Grid -> Position -> Int
calculateView grid position =
  viewLeft grid position * viewRight grid position * viewUp grid position * viewDown grid position

viewLeft :: Grid -> Position -> Int
viewLeft grid (x, y) = (if length row == visibleTrees then 0 else 1) + visibleTrees
  where
    row = reverse $ take y $ grid !! x
    tree = (grid !! x) !! y
    visibleTrees = length $ takeWhile (< tree) row

viewRight :: Grid -> Position -> Int
viewRight grid (x, y) = (if length row == visibleTrees then 0 else 1) + visibleTrees
  where
    row = drop (y + 1) $ grid !! x
    tree = (grid !! x) !! y
    visibleTrees = length $ takeWhile (< tree) row

viewUp :: Grid -> Position -> Int
viewUp grid (x, y) = viewLeft (transpose grid) (y, x)

viewDown :: Grid -> Position -> Int
viewDown grid (x, y) = viewRight (transpose grid) (y, x)

-- parse

parse :: String -> Grid
parse = lines
