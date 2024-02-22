module Main where

import AOC
import Data.Char
import Data.List

calibrate :: [String] -> Int
calibrate = sum . map (read . (\x -> [head x, last x]) . filter isDigit)

digitize :: [String] -> [String]
digitize = map go
  where
    go [] = []
    go line@(x : xs)
      | "one" `isPrefixOf` line = '1' : go xs
      | "two" `isPrefixOf` line = '2' : go xs
      | "three" `isPrefixOf` line = '3' : go xs
      | "four" `isPrefixOf` line = '4' : go xs
      | "five" `isPrefixOf` line = '5' : go xs
      | "six" `isPrefixOf` line = '6' : go xs
      | "seven" `isPrefixOf` line = '7' : go xs
      | "eight" `isPrefixOf` line = '8' : go xs
      | "nine" `isPrefixOf` line = '9' : go xs
      | otherwise = x : go xs

main :: IO ()
main = do
  dataFileName <- getDataFileName
  cals <- lines <$> readFile dataFileName
  print $ calibrate cals
  print $ calibrate $ digitize cals
