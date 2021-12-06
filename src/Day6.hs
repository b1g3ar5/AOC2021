module Day6 where

import Data.List (sort, group)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Utils (splitOn, getLines)


type Fish = Int


run :: Int -> [Int] -> Int
run 0 counts = sum counts
run days counts = run (days-1) $ go counts
  where
    go :: [Int] -> [Int]
    go [] = error "This should be impossible"
    go (z:others) = take 6 others ++ [z + others!!6, others!!7, z]


day6 :: IO ()
day6 = do
  inLines <- getLines 6
  let fish :: [Fish]
      fish = read <$> splitOn "," (head inLines)
      -- Count the incidences of each fish-life and add on missing life spans
      counts :: [Int]
      counts = 0:(length <$> group (sort fish)) ++ [0,0,0]
  putStrLn $ "Day6: part1: " ++ show (run 80 counts) -- 361169
  putStrLn $ "Day6: part1: " ++ show (run 256 counts) -- 1634946868992

  return ()
