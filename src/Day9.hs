
module Day9 where


import Utils (getLines, fromMaybe, Coord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List ((\\), sort, nub, partition, minimumBy, group)
import Data.Function
import Data.Maybe 


parseHeights :: [String] -> Map Coord Int
parseHeights ls = M.fromList $ concatMap (\(y,l) -> (\(x, c) -> ((x, y), read [c])) <$> zip [0..] l) $ zip [0..] ls


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


lab :: Map Coord Int -> (Coord -> Maybe Coord)
lab hMap = getLabel
  where
    isMin :: Coord -> Int -> Bool
    isMin p v = all (\n -> fromMaybe (error $ "Not in map: " ++ show n) (hMap M.!? n) > v) $ neighbours p

    getLabel :: Coord -> Maybe Coord
    getLabel c
      | hMap M.! c == 9 = Nothing
      | isMin c (hMap M.! c) = Just c
      | otherwise = getLabel $ fst $ minimumBy (compare `on` snd) $ (\n -> (n, hMap M.! n)) <$> ns
      where
        ns = neighbours c


day9 :: IO ()
day9 = do
  inLines <- getLines 9
  let heightMap = parseHeights inLines
      --sizes = label heightMap
      lf = lab heightMap
      cs = [(x,y) | x<-[0..maxX], y<-[0..maxY]]
  putStrLn $ "Day9: part1: " ++ show (countMins heightMap) 
  putStrLn $ "Day9: part2: " ++ show (product $ take 3 $ reverse $ sort $ length <$> group (sort (mapMaybe (lab heightMap) cs)))
  
  return ()


ls = ["2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"]