module Day14 where

import Utils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M 


type Rules = [((Char, Char), Char)]


parseRule :: String -> ((Char, Char), Char)
parseRule s = ((head $ head ps, head ps !! 1), head $ ps!!2)
  where
    ps = words s


type Count  = Map (Char, Char) Int


pairs :: String -> Count
pairs = go M.empty
  where
    go :: Count -> String -> Count
    go acc [] = acc
    go acc [c] = acc
    go acc (x:y:zs) = go (M.insertWith (+) (x,y) 1 acc) $ y:zs


apply :: Rules -> Count -> Count
apply rules mp = go M.empty rules
  where
    go newMap [] = newMap
    go newMap (((x1, x2), ins):rs) = go (M.insertWith (+) (x1, ins) n $ M.insertWith (+) (ins, x2) n newMap) rs
      where
        n = fromMaybe 0 $ mp M.!? (x1, x2)


score :: Count -> Int 
score p = last s - head s + 1
  where
  s = sort $ M.elems $ M.map (\n -> (n `div` 2) + 1) $ M.foldlWithKey insertPair M.empty p
  insertPair mp (x1,x2) n = M.insertWith (+) x1 n $ M.insertWith (+) x2 n mp

day14 :: IO ()
day14 = do
  ls <- getLines 14
  let start :: Count
      start = pairs $ head ls
      rules = parseRule <$> tail (tail ls)
  putStrLn $ "Day14: part1: " ++ show (score $ iterate (apply rules) start !! 10)
  putStrLn $ "Day14: part1: " ++ show (score $ iterate (apply rules) start !! 40)

  return ()

