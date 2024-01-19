module AOC.AOC2023.Day1 where

import Data.Char
import qualified System.Environment as Env
import Data.List

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

calibrate :: Input -> Output
calibrate = sum . map (read . (\x -> [head x, last x]) . filter isDigit)

digitize :: Input -> Input
digitize = map go
  where
    go [] = []
    go line@(x:xs)
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

compute :: Input -> String -> IO ()
compute input part = case part of
  "1" -> print $ calibrate input
  "2" -> print $ calibrate $ digitize input
  _ -> putStrLn "Invalid part"

run :: IO ()
run = do
  args <- Env.getArgs
  input <- parseInput <$> readFile (head args)
  mapM_ (compute input) (tail args)
