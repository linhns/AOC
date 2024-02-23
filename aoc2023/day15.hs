module Main where

import AOC (getDataFileName)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (ord)
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text as T (init, splitOn, unpack)
import qualified Data.Text.IO as TIO

hash :: String -> Int
hash = foldl' go 0
  where
    go acc c = ((acc + ord c) * 17) `mod` 256

part1 :: Text -> Int
part1 = sum . fmap (hash . T.unpack) . T.splitOn "," . T.init

data Lens = Lens {lensLabel :: String, lensFocalLength :: Int}
  deriving (Show, Eq)

data Operation = Remove String | Insert String Int
  deriving (Show, Eq)

type LensConfig = M.Map Int [Lens]

removeP :: Parser Operation
removeP = Remove <$> many1 letter <* "-"

insertP :: Parser Operation
insertP = Insert <$> many1 letter <* "=" <*> decimal

operationP :: Parser Operation
operationP = removeP <|> insertP

operationsP :: Parser [Operation]
operationsP = operationP `sepBy` ","

parseInput :: Text -> [Operation]
parseInput input =
  case parseOnly operationsP input of
    Left _ -> []
    Right ops -> ops

runOp :: LensConfig -> Operation -> LensConfig
runOp lc (Remove label) =
  M.adjust (filter ((/= label) . lensLabel)) (hash label) lc
runOp lc (Insert label fl)
  | any ((== label) . lensLabel) lensesInBox = M.insert hs newLensesForBox lc
  | otherwise = M.insert hs (lensesInBox <> [lens]) lc
  where
    hs = hash label
    lens = Lens label fl
    lensesInBox = M.findWithDefault [] hs lc
    newLensesForBox = fmap replaceLens lensesInBox
    replaceLens l
      | lensLabel l == label = lens
      | otherwise = l

runOps :: [Operation] -> LensConfig
runOps = foldl' runOp M.empty

boxPower :: Int -> [Lens] -> Int
boxPower boxNum lenses =
  (boxNum + 1) * sum (zipWith (*) [1 ..] (fmap lensFocalLength lenses))

configPower :: LensConfig -> Int
configPower = sum . M.elems . M.mapWithKey boxPower

part2 :: [Operation] -> Int
part2 = configPower . runOps

main :: IO ()
main = do
  dataFileName <- getDataFileName
  input <- TIO.readFile dataFileName
  print $ part1 input
  print $ part2 (parseInput input)
