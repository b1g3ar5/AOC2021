
module Day9 where


import Utils (getLines, fromMaybe, Coord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List ((\\), sort, nub, partition)


parseHeights :: [String] -> Map Coord Int
parseHeights ls = M.fromList $ concat $ (\(y,l) -> (\(x, c) -> ((x, y), read [c])) <$> zip [0..] l) <$> zip [0..] ls


maxX, maxY :: Int
maxX = 99
maxY = 99


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y)
  | x==0 && y==0 = [(x+1,y), (x,y+1)]
  | x==0 && y==maxY = [(x+1,y),(x,y-1)]
  | x==maxX && y==0 = [(x-1,y),(x,y+1)]
  | x==maxX && y==maxY = [(x-1,y),(x,y-1)]
  | x==0 = [(x+1,y),(x,y-1),(x,y+1)]
  | y==0 = [(x+1,y),(x-1,y),(x,y+1)]
  | x==maxX = [(x-1,y),(x,y-1),(x,y+1)]
  | y==maxY = [(x+1,y),(x-1,y),(x,y-1)]
  | otherwise = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]


countMins :: Map Coord Int -> Int
countMins heightMap = sum $ M.elems $ M.map (+1) $ M.filterWithKey isMin heightMap 
  where
    isMin :: Coord -> Int -> Bool
    isMin p v = all (\n -> fromMaybe (error $ "Not in map: " ++ show n) (heightMap M.!? n) > v) $ neighbours p


label :: Map Coord Int -> [Int]
label heightMap = go cs []
  where
    cs = M.keys $ M.filter (/= 9) heightMap
    go :: [Coord] -> [Int] -> [Int]
    go [] sizes = sizes
    go (k:ks) sizes = go rem (bsize:sizes)
      where
        (rem, bsize) = getBasin [k] ks 0

    getBasin :: [Coord] -> [Coord] -> Int -> ([Coord], Int)
    getBasin [] possibles size = (possibles, size)
    getBasin (c:cs) possibles size = getBasin (cs++ns) (possibles \\ ns) $ size+1
      where
        ns = filter (`elem` possibles) $ neighbours c


day9 :: IO ()
day9 = do
  inLines <- getLines 9
  let heightMap = parseHeights inLines
      sizes = label heightMap
  putStrLn $ "Day9: part1: " ++ show (countMins heightMap) 
  putStrLn $ "Day9: part2: " ++ show (product $ take 3 $ reverse $ sort sizes)
  
  return ()


