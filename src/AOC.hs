module AOC (getDataFileName) where

import System.Environment

fixPath :: String -> String
fixPath path = f $ break (== '-') path
  where
    f (a, b) = drop 1 b <> "/" <> a

getDataFileName :: IO String
getDataFileName = do
  args <- getArgs
  progName <- getProgName
  let dataFileName =
        if null args
          then "./data/" <> fixPath progName <> ".txt"
          else head args
  return dataFileName
