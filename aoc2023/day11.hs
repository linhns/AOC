module Main where

import AOC (getDataFileName)
import Data.List

type Grid = [[Char]]

parseInput :: String -> Grid
parseInput = lines

type Galaxy = (Int, Int)

getGalaxies :: Grid -> [Galaxy]
getGalaxies grid = [(row, col) | (row, col, char) <- coords, char == '#']
  where
    coords = concat $ zipWith (curry f) [0 :: Int ..] (map (zip [0 :: Int ..]) grid)
    f (_, []) = []
    f (row, (col, char) : xs) = (row, col, char) : f (row, xs)

getRealCoords :: Grid -> Int -> Galaxy -> Galaxy
getRealCoords grid coef (x, y) = (a, b)
  where
    a = x + (coef - 1) * length (filter (< x) emptyRows)
    b = y + (coef - 1) * length (filter (< y) emptyCols)
    emptyRows = getEmptyRows grid
    emptyCols = getEmptyRows $ transpose grid

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getEmptyRows :: Grid -> [Int]
getEmptyRows grid = [row | (row, line) <- zip [0 ..] grid, all (== '.') line]

findTotalShortestDistances :: Int -> Grid -> Int
findTotalShortestDistances coef grid = sum [manhattan a b | a <- realGalaxies, b <- realGalaxies, a < b]
  where
    galaxies = getGalaxies grid
    realGalaxies = map (getRealCoords grid coef) galaxies

part1 :: Grid -> Int
part1 = findTotalShortestDistances 2

part2 :: Grid -> Int
part2 = findTotalShortestDistances 1000000

main :: IO ()
main = do
  dataFileName <- getDataFileName
  grid <- parseInput <$> readFile dataFileName
  print $ part1 grid
  print $ part2 grid
