{-# LANGUAGE TupleSections #-}

module Main where

import AOC (getDataFileName)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Part = Part
  { x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
  }
  deriving (Show, Eq, Ord)

partP :: Parser Part
partP =
  Part
    <$> ("{x=" *> decimal)
    <*> (",m=" *> decimal)
    <*> (",a=" *> decimal)
    <*> (",s=" *> decimal <* "}")

partsP :: Parser [Part]
partsP = partP `sepBy` endOfLine

data Destination = Accept | Reject | Next String
  deriving (Show, Eq, Ord)

nameP :: Parser String
nameP = T.unpack <$> takeWhile1 isAsciiLower

destinationP :: Parser Destination
destinationP = choice [Accept <$ "A", Reject <$ "R", Next <$> nameP]

data Category = X | M | A | S
  deriving (Show, Eq, Ord)

categoryP :: Parser Category
categoryP = choice [X <$ "x", M <$ "m", A <$ "a", S <$ "s"]

data Comparator = Lt | Gt
  deriving (Show, Eq, Ord)

comparatorP :: Parser Comparator
comparatorP = choice [Lt <$ "<", Gt <$ ">"]

data Test = Test
  { category :: Category,
    comparator :: Comparator,
    value :: Int
  }
  deriving (Show, Eq, Ord)

testP :: Parser Test
testP = Test <$> categoryP <*> comparatorP <*> decimal

data Rule = WithoutTest Destination | WithTest Test Destination
  deriving (Show, Eq, Ord)

withoutTestP :: Parser Rule
withoutTestP = WithoutTest <$> destinationP

withTestP :: Parser Rule
withTestP = WithTest <$> testP <* ":" <*> destinationP

ruleP :: Parser Rule
ruleP = withTestP <|> withoutTestP

type Rules = [Rule]

rulesP :: Parser Rules
rulesP = ruleP `sepBy` ","

type Workflow = (String, Rules)

workflowP :: Parser Workflow
workflowP = (,) <$> nameP <* "{" <*> rulesP <* "}"

type Machine = Map String Rules

machineP :: Parser Machine
machineP = M.fromList <$> workflowP `sepBy` endOfLine

type Input = (Machine, [Part])

inputP :: Parser Input
inputP = (,) <$> (machineP <* endOfLine <* endOfLine) <*> partsP

successfulParse :: Text -> Input
successfulParse input =
  case parseOnly inputP input of
    Left err -> error err
    Right x -> x

sumRatings :: Part -> Int
sumRatings (Part x m a s) = x + m + a + s

satisfies :: Part -> Test -> Bool
satisfies part Test {..} = case comparator of
  Lt -> val < value
  Gt -> val > value
  where
    val = case category of
      X -> x part
      M -> m part
      A -> a part
      S -> s part

applyRules :: Part -> Rules -> Destination
applyRules _ [] = Reject -- at least one rule must match
applyRules part (rule : rules) =
  case rule of
    WithoutTest dest -> dest
    WithTest test dest ->
      if part `satisfies` test
        then dest
        else applyRules part rules

applyMachine :: Machine -> Part -> Destination
applyMachine machine part = go machine part "in"
  where
    go :: Machine -> Part -> String -> Destination
    go machine' part' name = case applyRules part (machine' M.! name) of
      Next next -> go machine' part' next
      dest -> dest

part1 :: [Part] -> Machine -> Int
part1 parts machine = sum $ fmap sumRatings acceptedParts
  where
    acceptedParts = filter ((== Accept) . applyMachine machine) parts

-- Part 2

getPred :: Test -> (Int -> Bool)
getPred (Test _ Lt val) = (< val)
getPred (Test _ Gt val) = (> val)

followToAcceptance :: Machine -> [Map Category [Int]]
followToAcceptance machine = go (Next "in") initial
  where
    initial = M.fromList $ map (,[1 .. 4000 :: Int]) [X, M, A, S]
    go :: Destination -> Map Category [Int] -> [Map Category [Int]]
    go Accept ranges = [ranges]
    go Reject _ = []
    go (Next next) ranges = tryPaths (machine M.! next) ranges
      where
        tryPaths [] _ = []
        tryPaths (r : rs) ranges' = case r of
          WithoutTest dest -> go dest ranges'
          WithTest test dest -> go dest trueRanges ++ tryPaths rs falseRanges
            where
              c = category test
              p = getPred test
              trueRanges = M.adjust (filter p) c ranges'
              falseRanges = M.adjust (filter (not . p)) c ranges'

part2 :: Machine -> Int
part2 = sum . map (product . map length . M.elems) . followToAcceptance

main :: IO ()
main = do
  dataFileName <- getDataFileName
  text <- TIO.readFile dataFileName
  let (machine, parts) = successfulParse text
  print $ part1 parts machine
  print $ part2 machine
