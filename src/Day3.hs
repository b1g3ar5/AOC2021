
module Day3 where

import Data.List (transpose, sort, group, sortOn, maximumBy)
import Data.Bifunctor
import Data.Ord
import Utils (getLines)


convertBinary :: [Char] -> Integer
convertBinary = go . reverse
  where
    go :: String -> Integer
    go [] = 0
    go (c:cs) = 2 * go cs + if c=='1' then 1 else 0


dup :: a -> (a,a)
dup x = (x,x)


data Selector = MostCommon | LeastCommon


choose :: Ord a => Selector -> [a] -> a
choose MostCommon xs = snd $ maximum $ bimap length head . dup <$> group (sort xs)
choose LeastCommon xs = snd $ minimum $ bimap length head . dup <$> group (sort xs)


select :: (Ord a, Eq a) => Selector -> [[a]] -> [a]
select sel = go 0 
  where
    -- n is the position on the string we are testing
    go _ [] = error "We are supposed to always have one string left"
    go n [r] = r
    go n rs = go (n+1) $ filter (\r -> r !! n == common) rs
      where
        common = choose sel $ (!!n) <$> rs


day3 :: IO ()
day3 = do
  rows <- getLines 3
  let len = length $ head rows
      common :: Integer
      common = convertBinary $ choose MostCommon <$> transpose rows

  putStrLn $ "Day3: part1: " ++ show (common*(2^len - 1 - common))
  putStrLn $ "Day3: part2: " ++ show (convertBinary (select MostCommon rows) * convertBinary (select LeastCommon rows))
  
  return ()


