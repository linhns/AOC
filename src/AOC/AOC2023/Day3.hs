{-# LANGUAGE TupleSections #-}

module AOC.AOC2023.Day3 where

import Data.Char
import Data.List
import Data.Matrix
import qualified System.Environment as Env

data Number = Number {value :: Int, row :: Int, span :: (Int, Int)}
  deriving (Show)

type Gear = (Int, Int)

parseGrid :: String -> Matrix Char
parseGrid = fromLists . lines

parseLine :: String -> [((Int, Int), String)]
parseLine = map (\l -> ((fst $ head l, fst $ last l), map snd l)) . groupConsecutive (\a b -> fst b - fst a <= 1) . filter (isDigit . snd) . zip [1 :: Int ..]

parseGear :: String -> [Gear]
parseGear = concatMap (\(r, cs) -> map (r,) cs) . zip [1 :: Int ..] . map ((map fst . filter ((== '*') . snd)) . zip [1 :: Int ..]) . lines

groupConsecutive :: (a -> a -> Bool) -> [a] -> [[a]]
groupConsecutive pred = foldr group []
  where
    group x [] = [[x]]
    group x acc@((h : t) : rest)
      | pred x h = (x : h : t) : rest
      | otherwise = [x] : acc

extractNumbersV2 :: String -> [Number]
extractNumbersV2 = concatMap (\(r, ds) -> map (\(s, d) -> Number (read d) r s) ds) . zip [1 :: Int ..] . map parseLine . lines

neighbours :: Matrix Char -> Number -> [Char]
neighbours grid (Number _ r (s, e)) =
  [ grid ! (i, j)
    | i <- [r - 1, r, r + 1],
      0 < i && i <= m,
      j <- [s - 1 .. e + 1],
      0 < j && j <= n
  ]
  where
    m = nrows grid
    n = ncols grid

neighbouringCoords :: Matrix Char -> (Int, Int) -> [(Int, Int)]
neighbouringCoords grid (r, c) =
  [ (i, j)
    | i <- [r - 1, r, r + 1],
      0 < i && i <= m,
      j <- [c - 1 .. c + 1],
      0 < j && j <= n,
      (i, j) /= (r, c)
  ]
  where
    m = nrows grid
    n = ncols grid

sumGearRatio :: Matrix Char -> [Number] -> [Gear] -> Int
sumGearRatio grid nums = sum . map (product . map value) . filter ((== 2) . length) . map getAdjacentNumbers
  where
    getAdjacentNumbers g =
      filter
        ( \(Number _ r (cb, ce)) ->
            any (`elem` neighbouringCoords grid g) [(r, c) | c <- [cb .. ce]]
        )
        nums

isPartNumber :: (Foldable t) => t Char -> Bool
isPartNumber = any (\c -> not (isDigit c) && c /= '.')

sumPartNumbers :: Matrix Char -> [Number] -> Int
sumPartNumbers grid nums = sum . map value $ filter (isPartNumber . neighbours grid) nums

compute :: String -> String -> IO ()
compute input part = case part of
  "1" -> print $ sumPartNumbers (parseGrid input) (extractNumbersV2 input)
  "2" -> print $ sumGearRatio (parseGrid input) (extractNumbersV2 input) (parseGear input)
  _ -> putStrLn "Invalid part"

run :: IO ()
run = do
  args <- Env.getArgs
  input <- readFile (head args)
  mapM_ (compute input) (tail args)
