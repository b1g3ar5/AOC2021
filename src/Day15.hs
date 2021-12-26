{-# LANGUAGE FlexibleInstances #-}

module Day15 where

import Debug.Trace

import Utils (getLines, Coord)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><))
import Data.Foldable (foldl', sum)
import Data.Char
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Array.IArray
import Algorithm.Search

type Position = Coord
type TiledPosition = Coord
type Grid = Array Coord Int

data Cave = Cave 
  { _grid :: Grid
  , _goal :: Coord
  } deriving (Eq, Ord, Show)
type CaveContext = Reader Cave


data State = State { _position :: Coord
                   , _trail :: Q.Seq Coord
                   , _trailCost :: Int
                   , _cost :: Int
                   --, _grid :: Grid
                   , _visited :: S.Set Coord
                   --, _goal :: Coord
                   } deriving (Show, Eq)
type Path = P.MinPQueue Int State


emptySearchState :: Coord
emptySearchState = (0, 0)


estimateCost :: Coord -> CaveContext Int
estimateCost here = do 
  goal <- asks _goal
  let (dr, dc) = here - goal
  return $ abs dr + abs dc


isGoal :: Coord -> CaveContext Bool
isGoal here = do 
  goal <- asks _goal
  return $ here == goal


entryCost :: Coord -> CaveContext Int
entryCost here = do 
  grid <- asks _grid
  return $ grid ! here


successors1 :: Position -> CaveContext (Q.Seq Position)
successors1 here = do 
  grid <- asks _grid
  let neighbours = filter (inRange (bounds grid))  
            [ here + delta | delta <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
  let succs = Q.fromList neighbours
  return succs


successors2 :: Position -> CaveContext (Q.Seq Position)
successors2 here = do 
  grid <- asks _grid
  let (lowBound, highBound) = bounds grid
  let extendedBounds = (lowBound , tileScale highBound)
  let neighbours = filter (inRange extendedBounds)  
              [ here + delta | delta <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
  let succs = Q.fromList neighbours
  return succs


tileScale :: Coord -> Coord
tileScale (r, c) = (ts r, ts c)
  where ts n = (n + 1) * 5 - 1


day15 :: IO ()
day15 = 
  do  text <- getLines 15 
      let cave = mkCave $ unlines text
      print $ part1 cave
      print $ part2 cave


mkCave :: String -> Cave
mkCave text = Cave { _grid = grid, _goal = (r, c) }
  where rows = lines text
        r = length rows - 1
        c = length (head rows) - 1
        grid = listArray ((0, 0), (r, c)) $ map mkCell $ concat rows
        mkCell e = digitToInt e

  
part1 :: Cave -> Int
part1 cave = maybe 0 _cost result
  where 
    result = runReader searchCave cave :: Maybe State

part2 :: Cave -> Int
part2 cave = maybe 0 _cost result
  where
    result = runReader searchCave cave :: Maybe State


searchCave ::  CaveContext (Maybe State)
searchCave = do
  path <- initPath
  aStar' path S.empty


initPath ::  CaveContext Path
initPath = do 
  let ss = emptySearchState
  c <- estimateCost ss
  return $ P.singleton c State { _position = ss, _trail = Q.empty, _trailCost = 0, _cost = c}


aStar' ::  Path -> S.Set Coord -> CaveContext (Maybe State)
aStar' path visited 
  | P.null path = return Nothing
  | otherwise = do  
      let (_, currentState) = P.findMin path
      let reached = _position currentState 
      nexts <- candidates currentState visited
      let newPath = foldl' (\q a -> P.insert (_cost a) a q) (P.deleteMin path) nexts
      reachedGoal <- isGoal reached
      if reachedGoal
      then return (Just currentState)
      else if reached `S.member` visited
           then aStar' (P.deleteMin path) visited
           else aStar' newPath (S.insert reached visited)


myStar :: Path -> S.Set Coord -> CaveContext (Maybe State)
myStar path visited
  | P.null path = return Nothing
  | otherwise = do
      let (_, currentState) = P.findMin path
      moves <- candidates currentState visited

      --aStar (state -> f state) (state -> state -> cost) (state -> cost) (state -> Bool) state
      return undefined


candidates :: State -> S.Set Coord -> CaveContext (Q.Seq State)
candidates state visited = do  
  succs <- successors1 $ _position state
  let noloops = Q.filter (`S.notMember` visited) succs
  mapM (makeState (_trail state) (_trailCost state)) noloops


makeState ::  Q.Seq Coord -> Int -> Coord -> CaveContext State
makeState previous prevCost newPosition = do
  predicted <- estimateCost newPosition
  grid <- asks _grid
  newPositionCost <- entryCost newPosition
  return State { _position = newPosition
               , _trail = previous |> newPosition
               , _trailCost = prevCost + newPositionCost
               , _cost = prevCost + newPositionCost + predicted
               }
                      
