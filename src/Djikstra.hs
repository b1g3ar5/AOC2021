{-# LANGUAGE FlexibleInstances #-}

module Djikstra (
    dijkstra
    , search
  ) where


--import Utils
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><)) 
import Data.Foldable (foldl', sum)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Map (Map(..), (!))


search :: (Ord s, Ord k, Num k) => (s, k) -> (s -> Q.Seq s) -> (s -> k) -> (s -> Bool) -> Maybe k
search (start, startCost) next cost finished = dijkstra next cost finished (P.singleton startCost start) S.empty
  

dijkstra :: (Ord a, Num k, Ord k) => (a -> Q.Seq a) -> (a -> k) -> (a -> Bool) -> P.MinPQueue k a -> S.Set a -> Maybe k
dijkstra nextStates cost isFinished pipeline visited 
  | P.null pipeline = Nothing
  | isFinished position = Just currentCost
  | position `S.member` visited = dijkstra nextStates cost isFinished (P.deleteMin pipeline) visited
  | otherwise = dijkstra nextStates cost isFinished newPipeline (S.insert position visited)
  where
    --(currentCost, currentState) = P.findMin pipeline
    ((currentCost, currentState), remainingPipeline) = P.deleteFindMin pipeline
    position = currentState
    nexts = Q.filter (`S.notMember` visited) $ nextStates currentState
    newPipeline = foldl' (\q next -> P.insert (currentCost + cost next) next q) remainingPipeline nexts


