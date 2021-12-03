
module Day3 where

import Data.List (transpose, sort, group)
import Utils (getLines, toInt)


choose :: (Eq a, Ord a) => ([(Int, a)] -> (Int,a)) -> [a] -> a
choose f xs = snd $ f $ (\g -> (length g, head g)) <$> group (sort xs)


convert :: [Char] -> Integer
convert = toInt . reverse . ((=='1') <$>)


select :: (Ord a, Eq a) => ([(Int, a)] -> (Int,a)) -> [[a]] -> [a]
select f = go 0 
  where
    go n [r] = r
    go n rs = go (n+1) $ filter (\r -> r !! n == mc) rs
      where
        mc = choose f $ (!!n) <$> rs


day3 :: IO ()
day3 = do
  rows <- getLines 3
  let len = length $ head rows
      common :: Integer
      common = convert $ choose maximum <$> transpose rows

  putStrLn $ "Day3: part1: " ++ show (common*(2^len - 1 - common))
  putStrLn $ "Day3: part2: " ++ show (convert (select maximum rows) * convert (select minimum rows))
  
  return ()


