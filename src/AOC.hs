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
  let baseDataName =
        if null args
          then fixPath progName
          else head args
  let dataFileName = "./data/" ++ baseDataName ++ ".txt"
  return dataFileName
