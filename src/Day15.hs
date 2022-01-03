{-# LANGUAGE DataKinds, FlexibleInstances #-}

module Day15 where


import Prelude hiding (lookup)
import Utils (Set, Map, fromJust, getLines, neighbours4, Coord, isNothing)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import HeapIndexed (Heap, singleton, union, fromList, extractMin)
import Data.Type.Bool (Not)


type Grid = Map Coord Dist
type Dist = Maybe Int
type State = (Grid, Heap Dist Coord, Set Coord, Coord -> Bool)


-- Make Nothing represent infinity
-- <> is addition
-- mempty is infinity so that ord/min works
instance {-# overlapping #-} Semigroup Dist where
  Nothing <> Nothing = Nothing 
  Nothing <> _ = Nothing 
  _ <> Nothing = Nothing 
  Just x <> Just y = Just $ x+y


instance {-# overlapping #-} Monoid Dist where
  mempty = Nothing


instance {-# overlapping #-} Ord Dist where
  Nothing <= Nothing = False
  Nothing <= Just _ = False
  Just _ <= Nothing = False
  Just x <= Just y = x <= y


parseGrid :: [String] -> Grid
parseGrid ess = M.fromList  $ concat $ (\(y, es) -> (\(x, e) -> ((x,y), Just $ read [e])) <$> zip [0..(gridSize1-1)] es) <$> zip [0..(gridSize1-1)] ess


minMaybe :: (Monoid a, Ord a) => Maybe a -> Maybe a -> a
minMaybe Nothing Nothing = mempty
minMaybe (Just x) Nothing = x
minMaybe Nothing (Just x) = x
minMaybe (Just x) (Just y) = min x y


floodFill :: Grid -> Grid
floodFill g = foldl go (M.singleton (0,0) $ Just 0) [1..(gridSize1-1)]
  where
    
    go :: Grid -> Int -> Grid
    go mg level = mg3
      where
        mg1 = M.insert (level, 0) (g M.! (level, 0) <> mg M.! (level-1,0))
            $ M.insert (0, level) (g M.! (0, level) <> mg M.! (0, level-1)) mg
        mg2 = foldl (\acc p@(x,y) 
                      -> M.insert p (g M.! p <> minMaybe (acc M.!? (x-1,y)) (acc M.!? (x,y-1))) acc
                    ) mg1 $ (level,) <$> [1..(level-1)]
        mg3 = foldl (\acc p@(x,y) 
                      -> M.insert p (g M.! p <> minMaybe (acc M.!? (x, y-1)) (acc M.!? (x-1, y))) acc
                    ) mg2 $ (,level) <$> [1..level]


lookup :: Map Coord Dist  -> Coord -> Dist
lookup g (x, y)
  | isNothing base = Nothing 
  | otherwise = Just $ 1 + mod (fromJust base + qx + qy - 1) 9
  where
    (qx, rx) = divMod x 100
    (qy, ry) = divMod y 100
    base = g M.! (rx, ry)


-- Global grid sizes
gridSize1, gridSize2 :: Int
gridSize1 = 100
gridSize2 = 500
target1, target2 :: Coord
target1 = (gridSize1-1, gridSize1-1)
target2 = (gridSize2 - 1, gridSize2 - 1)
inBounds1, inBounds2 :: Coord -> Bool
inBounds1 (x,y) = x>=0 && y>=0 && x < gridSize1 && y < gridSize1
inBounds2 (x,y) = x>=0 && y>=0 && x < gridSize2 && y < gridSize2


dijkstra :: State -> Dist
dijkstra (grid, pipeline, visited, finished)
  | null pipeline = mempty
  | finished position = savedMin
  | position `S.member` visited = dijkstra (grid, remainingPipeline, visited, finished)
  | otherwise = dijkstra (grid, newPipeline, S.insert position visited, finished)
  where
    ((savedMin, position), remainingPipeline) = fromJust $ extractMin pipeline
    ns = filter inBounds2 $ filter (`S.notMember` visited) $ neighbours4 position
    newPipeline = remainingPipeline `union` fromList ((\n -> (savedMin <> (grid `lookup` n ), n)) <$> ns)


day15 :: IO ()
day15 = do
  ls <- getLines 15
  let grid = parseGrid ls
      mg1 = floodFill grid
      startState :: State
      startState = (grid, singleton (Just 0) (0,0), S.empty, (==target2))

  putStrLn $ "Day15:part1: " ++ show (mg1 M.! target1)
  putStrLn $ "Day15:part1: " ++ show (dijkstra startState)
  
  return ()
