module Paths where

import Prelude hiding (Right, Left)
import Utils
import Data.Array
import Data.Maybe
import Data.Tree
import Data.Sequence (viewl, ViewL (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Utils

test = ["1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581"
  ]


type Board = Array (Int, Int) Int

gridSize :: Int
gridSize = 10

parseGrid :: [String] -> Board
parseGrid ess = listArray ((0,0),(gridSize,gridSize)) $ concat $ (\es -> (\e -> read [e]) <$> es) <$> ess


board :: Board
board = parseGrid test


(!?) :: Ix i => Array i a -> i -> Maybe a
a !? i = if inRange (bounds a) i then Just (a ! i) else Nothing


--data State = State {position :: (Int, Int), direction  :: (Int, Int)} deriving (Eq, Ord, Show)

right :: Num a => (a, a) -> (a, a)
right (down, across) = (across, -down)

left ::  Num a => (a, a) -> (a, a)
left (down, across) = (-across, down)

moveTowards :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
moveTowards (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


data Move = Left | Right | Forward | Jump deriving (Show)

inBounds :: Coord -> Bool
inBounds (x,y) = x>=0 && y>=0 && x<=gridSize-1 && y<=gridSize-1

moves :: Board -> Coord -> [Coord]
moves board pos = filter inBounds $ neighbours4 pos  


explore :: Board -> Coord -> [Tree Coord]
explore board = map go . moves board
  where
    go pos = Node pos (explore board pos)


limit :: Int -> Tree a -> Tree a
limit n (Node a ts)
  | n <= 0    = Node a []
  | otherwise = Node a (map (limit (n-1)) ts)


breadthFirstSearch :: (Coord -> Bool) -> [Tree Coord] -> Maybe [Coord]
breadthFirstSearch p = combine Seq.empty []
  where
    --combine :: Seq ([a], Tree a) -> [a] -> [Tree a] -> Maybe [a]
    combine queue ancestors branches = go (queue >< (Seq.fromList . map (ancestors,) $ branches))
    --go :: Seq ([a], Tree a) -> Maybe [a]
    go queue =
      case viewl queue of
        EmptyL -> Nothing
        (ancestors, Node a bs) :< queued ->
          if p a
          then Just . reverse $ a:ancestors
          else combine queued (a:ancestors) bs


solve :: Int -> Board -> Coord -> Maybe [Int]
solve goal board = fmap (map fst) . breadthFirstSearch ((== goal) . (board !)) . explore board


breadthFirstSearchUnseen:: Ord a => (a -> Bool) -> [Tree a] -> Maybe [a]
breadthFirstSearchUnseen p = combine Set.empty Seq.empty []
  where
    combine seen queued ancestors unseen =
      go
        (seen  `Set.union` (Set.fromList . map rootLabel $ unseen))
        (queued ><         (Seq.fromList . map (ancestors,) $ unseen))
    go seen queue =
      case viewl queue of
        EmptyL -> Nothing
        (ancestors, Node a bs) :< queued ->
          if p a
          then Just . reverse $ ancestors'
          else combine seen queued ancestors' unseen
          where
            ancestors' = a:ancestors
            unseen = filter (flip Set.notMember seen . rootLabel) bs


solve' :: (Int, Int) -> Board -> Coord -> Maybe [Int]
--solve' goal board = fmap (map fst) . breadthFirstSearchUnseen snd ((== goal) . (board !) . position . snd) . explore board
solve' goal board = fmap (map fst) . breadthFirstSearchUnseen (== goal) . explore board


