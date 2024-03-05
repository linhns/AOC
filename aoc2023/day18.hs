module Main where

import AOC (getDataFileName)
import Data.Attoparsec.Text as AT hiding (D)
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO

data Direction = U | D | L | R deriving (Show, Eq)

data Move = Move
  { direction :: !Direction,
    distance :: !Int
  }
  deriving (Show, Eq)

type Point = (Int, Int)

directionP :: Parser Direction
directionP = choice [U <$ "U", D <$ "D", L <$ "L", R <$ "R"]

moveP :: Parser Move
moveP =
  Move
    <$> directionP
    <* " "
    <*> decimal
    <* " "
    <* skipWhile (not . isSpace)

planP :: Parser [Move]
planP = moveP `sepBy` endOfLine

successfulParse :: Parser [Move] -> Text -> [Move]
successfulParse parser text =
  case parseOnly parser text of
    Left _ -> []
    Right x -> x

move :: Point -> Move -> Point
move (x, y) (Move U d) = (x - d, y)
move (x, y) (Move D d) = (x + d, y)
move (x, y) (Move L d) = (x, y - d)
move (x, y) (Move R d) = (x, y + d)

dig :: Point -> [Move] -> [Point]
dig = scanl' move

perimeter :: [Move] -> Int
perimeter = sum . fmap distance

area :: [Point] -> Int
area points = (`div` 2) . abs . sum . fmap shoelace $ zip points (tail points)
  where
    shoelace ((x1, y1), (x2, y2)) = x1 * y2 - x2 * y1

part1 :: [Move] -> Int
part1 ms = area points + (perimeter ms `div` 2) + 1
  where
    points = dig (0, 0) ms

direction2P :: Parser Direction
direction2P = choice [U <$ "3", D <$ "1", L <$ "2", R <$ "0"]

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

-- Part 2
move2P :: Parser Move
move2P = do
  _ <- directionP
  _ <- " "
  _ <- decimal @Int
  _ <- " (#"
  d <- AT.take 5
  dir <- direction2P
  _ <- ")"
  return $ Move dir (read (T.unpack ("0x" <> d)) :: Int)

plan2P :: Parser [Move]
plan2P = move2P `sepBy` endOfLine

main :: IO ()
main = do
  dataFileName <- getDataFileName
  input <- TIO.readFile dataFileName
  let plan1 = successfulParse planP input
  print $ part1 plan1
  let plan2 = successfulParse plan2P input
  print $ part1 plan2
