module Main where

import AOC (getDataFileName)
import Data.Attoparsec.Text hiding (take)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

data Card = Card
  { winnings :: [Int]
  , deck :: [Int]
  }
  deriving (Show, Eq)

getMatches :: Card -> Int
getMatches (Card ws cs) = length $ filter (`elem` ws) cs

{- | Score a card.
>>> score (Card [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53])
8
-}
score :: Card -> Int
score c
  | matches == 0 = 0
  | otherwise = 2 ^ (matches - 1)
 where
  matches = getMatches c

numbersP :: Parser [Int]
numbersP = decimal `sepBy` skipSpace

cardP :: Parser Card
cardP =
  Card
    <$ "Card"
    <* skipSpace
    <* decimal @Int
    <* ":"
    <* skipSpace
    <*> numbersP
    <* skipSpace
    <* "|"
    <* skipSpace
    <*> numbersP

cardsP :: Parser [Card]
cardsP = cardP `sepBy` endOfLine

successfulParse :: Text -> [Card]
successfulParse input = case parseOnly cardsP input of
  Left _ -> []
  Right x -> x

part1 :: [Card] -> Int
part1 = sum . fmap score

part2 :: [Card] -> Int
part2 cards = sum . go (repeat 1) $ fmap getMatches cards
 where
  go (cnt : cnts) (c : cs) = cnt : go (fmap (+ cnt) (take c cnts) <> drop c cnts) cs
  go _ _ = []

main :: IO ()
main = do
  dataFileName <- getDataFileName
  input <- TIO.readFile dataFileName
  let cards = successfulParse input
  print $ part1 cards
  print $ part2 cards
