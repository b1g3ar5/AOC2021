
module Day3 where

import Data.List (transpose, sort, group, sortOn, maximumBy)
import Data.Bifunctor
import Data.Ord
import Utils (getLines)


-- Converts a string 0f [1|0] to an integer
convert :: [Char] -> Integer
convert = go . reverse
  where
    go :: String -> Integer
    go [] = 0
    go (c:cs) = 2 * go cs + if c=='1' then 1 else 0


-- Duplicate a thing iinto a tuple
dup :: a -> (a,a)
dup x = (x,x)


-- I'm sure coose and select can be cleaned up somehow
-- by using Down or something else from Data.Ord

-- Here maximum will get the most commom a in the list
choose :: Ord d => ([(Int, d)] -> (a, b)) -> [d] -> b
choose f xs = snd $ f $ bimap length head . dup <$> group (sort xs)


select :: (Ord a, Eq a) => ([(Int, a)] -> (Int,a)) -> [[a]] -> [a]
select f = go 0 
  where
    go _ [] = error "We are supposed to always have one string left"
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


