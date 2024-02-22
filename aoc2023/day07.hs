module Main where

import AOC
import Control.Applicative
import Data.Attoparsec.Text
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)

data Hand = Hand {cards :: [Card], bid :: Int}
  deriving (Show, Eq, Ord)

data HandType
  = HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord, Enum)

data ClassifiedHand = ClassifiedHand HandType [Card] Int
  deriving (Show, Eq, Ord)

type SignatureElement = (Int, [Card])

type Signature = [SignatureElement]

sign :: [Card] -> Signature
sign = sortBy (comparing Down) . fmap (\x -> (length x, x)) . group . sort

-- Add jokers to the highest card
signJokers :: [Card] -> Signature
signJokers cards = addJokers nonJokersSigned (length jokers, jokers)
  where
    (jokers, nonJokers) = partition (== Joker) cards
    nonJokersSigned = sortBy (comparing Down) (fmap (\x -> (length x, x)) $ group $ sort nonJokers)

classify :: ([Card] -> Signature) -> Hand -> ClassifiedHand
classify f Hand {..} =
  case f cards of
    (5, _) : _ -> ClassifiedHand FiveOfAKind cards bid
    (4, _) : _ -> ClassifiedHand FourOfAKind cards bid
    (3, _) : (2, _) : _ -> ClassifiedHand FullHouse cards bid
    (3, _) : _ -> ClassifiedHand ThreeOfAKind cards bid
    (2, _) : (2, _) : _ -> ClassifiedHand TwoPair cards bid
    (2, _) : _ -> ClassifiedHand Pair cards bid
    _otherwise -> ClassifiedHand HighCard cards bid

part1 :: [Hand] -> Int
part1 hands = sum $ fmap score $ zip [1 ..] $ sort $ fmap (classify sign) hands
  where
    score (r, ClassifiedHand _ _ b) = r * b

part2 :: [Hand] -> Int
part2 hands =
  sum $
    fmap score $
      zip [1 ..] $
        sort $
          fmap (classify signJokers . jokerizeHand) hands
  where
    score (r, ClassifiedHand _ _ b) = r * b

replace :: (Eq a) => a -> a -> [a] -> [a]
replace from to = fmap (\x -> if x == from then to else x)

jokerize :: [Card] -> [Card]
jokerize = replace Jack Joker

jokerizeHand :: Hand -> Hand
jokerizeHand (Hand cards bid) = Hand (jokerize cards) bid

addJokers :: Signature -> SignatureElement -> Signature
addJokers [] js = [js]
addJokers ((n, cs) : xs) (jn, js) = (n + jn, cs ++ js) : xs

cardP :: Parser Card
cardP =
  (Two <$ "2")
    <|> (Three <$ "3")
    <|> (Four <$ "4")
    <|> (Five <$ "5")
    <|> (Six <$ "6")
    <|> (Seven <$ "7")
    <|> (Eight <$ "8")
    <|> (Nine <$ "9")
    <|> (Ten <$ "T")
    <|> (Jack <$ "J")
    <|> (Queen <$ "Q")
    <|> (King <$ "K")
    <|> (Ace <$ "A")

cardsP :: Parser [Card]
cardsP = many1 cardP

handP :: Parser Hand
handP = Hand <$> cardsP <* space <*> decimal

handsP :: Parser [Hand]
handsP = handP `sepBy` endOfLine

parseInput :: T.Text -> [Hand]
parseInput input =
  case parseOnly handsP input of
    Left _ -> []
    Right x -> x

main :: IO ()
main = do
  dataFileName <- getDataFileName
  hands <- parseInput <$> TIO.readFile dataFileName
  print $ part1 hands
  print $ part2 hands