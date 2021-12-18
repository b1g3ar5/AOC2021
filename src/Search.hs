module Search where

import Utils
import qualified Data.Map.Strict as M
import Debug.Trace

gridSize :: Int
gridSize = 10
target1, target2 :: (Int, Int)
target1 = (gridSize-1, gridSize-1)
target2 = (5*gridSize - 1, 5*gridSize - 1)
inBounds :: Coord -> Bool
inBounds (x,y) = x>=0 && y>=0 && x<=gridSize-1 && y<=gridSize-1

type Path = ([Move], State)
type Frontier = [Path]
type Stat = Int -- for example the length of the path
type State = (Coord, Stat, Map Coord Stat)
type Move = Coord

moves :: State -> [Coord]
moves (c, dist, mp) = filter inBounds $ neighbours4 c
move :: State -> Coord -> State
move (p, dist, mp) mv = (mv, dist + mp M.! mv, mp)
solved :: State -> Bool
solved (c, _, _) = c==target1


succs :: Path -> [Path]
succs (ms, q) = [(ms ++ [m], move q m) | m <- moves q]


bfSearch :: [State] -> Frontier -> Maybe [Move]
bfSearch qs [] = Nothing
bfSearch qs (p@(ms,q):ps)
  | solved q = Just ms
  | q `elem` qs = bfSearch qs ps
  | otherwise = trace (show p) $ bfSearch (q:qs) (ps ++ succs p)


bfSearch' :: [State] -> Frontier -> Frontier -> Maybe [Move]
bfSearch' qs [] [] = Nothing
bfSearch' qs rs [] = bfSearch' qs [] rs
bfSearch' qs rs (p@(ms,q):ps)
  | solved q = Just ms
  | q `elem` qs = bfSearch' qs rs ps
  | otherwise = bfSearch' (q:qs) (succs p ++ rs) ps

bfSearch'' :: [State] -> [Frontier] -> Frontier -> Maybe [Move]
bfSearch'' qs [] [] = Nothing
bfSearch'' qs pss [] = bfSearch'' qs [] $ concat $ reverse pss
bfSearch'' qs pss (p@(ms,q):ps)
  | solved q = Just ms
  | q `elem` qs = bfSearch'' qs pss ps
  | otherwise = bfSearch'' (q:qs) (succs p : pss) ps  

