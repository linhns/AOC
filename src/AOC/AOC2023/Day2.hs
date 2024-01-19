module AOC.AOC2023.Day2 where

import qualified System.Environment as Env
import Text.Regex.TDFA

data Cube = Red Int | Green Int | Blue Int deriving (Show, Eq)

type Game = (Int, [Cube])

parseInput :: String -> [[Cube]]
parseInput = map (map makeCube . matchCube) . lines

matchCube :: String -> [[String]]
matchCube s = s =~ "([0-9]+) (red|green|blue)" :: [[String]]

makeCube :: [String] -> Cube
makeCube [_, n, "red"] = Red (read n)
makeCube [_, n, "green"] = Green (read n)
makeCube [_, n, "blue"] = Blue (read n)

makeGames :: [[Cube]] -> [Game]
makeGames = zip [1..]

validateGame :: Game -> Bool
validateGame = all validateColor . snd
  where
    validateColor (Red n) = n <= 12
    validateColor (Green n) = n <= 13
    validateColor (Blue n) = n <= 14

sumValidGameIds :: [Game] -> Int
sumValidGameIds = sum . map fst . filter validateGame

minimalCubes :: [Cube] -> (Int, Int, Int)
minimalCubes cubes = (r cubes, g cubes, b cubes)
  where
    r = maximum . map (\(Red n) -> n) . filter isRed
    g = maximum . map (\(Green n) -> n) .  filter isGreen
    b = maximum . map (\(Blue n) -> n) . filter isBlue
    isRed (Red _) = True
    isRed _ = False
    isGreen (Green _) = True
    isGreen _ = False
    isBlue (Blue _) = True
    isBlue _ = False

power :: (Int, Int, Int) -> Int
power (x, y, z) = x * y * z

sumPowers :: [Game] -> Int
sumPowers = sum . map (power . minimalCubes . snd)

compute :: [[Cube]] -> String -> IO ()
compute input part = case part of
  "1" -> print $ sumValidGameIds $ makeGames input
  "2" -> print $ sumPowers $ makeGames input
  _ -> putStrLn "Invalid part"

run :: IO ()
run = do
  args <- Env.getArgs
  input <- parseInput <$> readFile (head args)
  mapM_ (compute input) (tail args)
