{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Day11 where


import Utils (Map, getLines, neighbours8, steadyState, Coord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


type Grid = Map Coord (Int, Int)


parseGrid :: [String] -> Grid
parseGrid ess = M.fromList $ concat $ (\(y, es) -> (\(x, e) -> ((x,y), (0, read [e]))) <$> zip [0..] es) <$> zip [0..] ess


play :: Grid -> Grid
play g = steadyState (\lg -> M.mapWithKey (update lg) lg) g1
  where
    g1 = M.map (\(c,v) -> (c, v+1)) g
    update :: Grid -> Coord -> (Int, Int) -> (Int, Int)
    update ug k (f,v)
      | v == 0 = (f, 0)
      | v>9 = (f+1, 0)
      | otherwise = (f, v + flashCount k ug)


flashCount :: Coord -> Grid -> Int
flashCount k g = length $ filter (\c -> c `elem` M.keys g && snd (g M.! c) > 9) (neighbours8 k)


simultaneousFlash :: Int -> Grid -> Int
simultaneousFlash n g
  | sum (M.map snd g) == 0 = n
  | otherwise = simultaneousFlash (n+1) $ play g


day11 :: IO ()
day11 = do
  inLines <- getLines 11
  let grid = parseGrid inLines
      
  putStrLn $ "Day11: part1: " ++ show (sum $ M.map fst $ iterate play grid !! 100)
  putStrLn $ "Day11: part2: " ++ show (simultaneousFlash 0 grid)

  return ()

