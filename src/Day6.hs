module Day6 where

import Data.List (sort, group)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Utils (splitOn, getLines)

-- Initial version with a list of the number of fish of each age [0..8]

run' :: Int -> [Int] -> Int
run' 0 fs = sum fs
run' days freqs = run' (days-1) $ go freqs
  where
    go :: [Int] -> [Int]
    go [] = error "This should be impossible"
    go (z:others) = take 6 others ++ [z + others!!6, others!!7, z]


day6' :: IO ()
day6' = do
  inLines <- getLines 6
  let fish :: [Int]
      fish = read <$> splitOn "," (head inLines)
      counts :: [Int]
      counts = (\i -> length $ filter (==i) fish) <$> [0..8]
  putStrLn $ "Day6: part1: " ++ show (run' 80 counts) -- 361,169
  putStrLn $ "Day6: part2: " ++ show (run' 256 counts) -- 1,634,946,868,992

  return ()

-- Now the MultiSet this is sometimes called a bag.
-- It keeps track of the counts for you so you can just forget all about them
-- but still get the performance as though you were trcking them...

run :: Int -> MultiSet Int -> Int
run 0 ms = MS.size ms
run days ms = run (days-1) $ go ms
  where
    go :: MultiSet Int -> MultiSet Int
    go ms = MS.concatMap (\e -> if e==0 then [6, 8] else [e-1]) ms


day6 :: IO ()
day6 = do
  inLines <- getLines 6
  let fish :: MultiSet Int
      fish = MS.fromList $ read <$> splitOn "," (head inLines)
  putStrLn $ "Day6: part1: " ++ show (run 80 fish)
  putStrLn $ "Day6: part2: " ++ show (run 256 fish)

  return ()

