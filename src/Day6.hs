module Day6 where

import Data.List (sort, group)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Utils (splitOn, getLines)


run :: Int -> [Int] -> Int
run 0 freqs = sum freqs
run days freqs = run (days-1) $ go freqs
  where
    go :: [Int] -> [Int]
    go [] = error "This should be impossible"
    go (z:others) = take 6 others ++ [z + others!!6, others!!7, z]


day6 :: IO ()
day6 = do
  inLines <- getLines 6
  let fish :: [Int]
      fish = read <$> splitOn "," (head inLines)
      counts :: [Int]
      counts = (\i -> length $ filter (==i) fish) <$> [0..8]
  putStrLn $ "Day6: part1: " ++ show (run 80 counts) -- 361,169
  putStrLn $ "Day6: part2: " ++ show (run 256 counts) -- 1,634,946,868,992

  return ()
