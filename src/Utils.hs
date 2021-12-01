{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Utils (
  getLines
  , getRaw
  , getWords
  , getParagraphs
  , splitOn
  , toInt
  , fromInt
  , pad
  , Coord
  , allCoords
  , neighbourCoords
  , splitAt
  , intersections
  , clockTurn
  , antiTurn
  , directions
  , leftOf
  , rightOf
  , above
  , below
  , neighbours
  , span
  , race
  , fixpoint
)
where


import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq(..), (><), (|>), (<|))
import Data.List
import Data.List.Split
import Data.Bifunctor
import Data.Tuple
import Data.Maybe
import Control.Monad
import Control.Monad.ST (runST, ST(..))
import System.TimeIt ( timeIt )

--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

------------ GET THE INPUT FROM FILE ------------------

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".in"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getParagraphs :: Int -> IO [[String]]
getParagraphs = getF (filter (/=[]) . splitOnChar "" . lines)


------------------ VARIOUS UTILITY FUNCTIONS --------------------


intersections :: Ord a => [S.Set a] -> S.Set a
intersections ss = foldl' S.intersection (head ss) (tail ss)


fix :: Eq a => (a -> a) -> a
fix f = x where x = f x


-- Should this just call fix somehow?
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x 
  | x ==fx = fx
  | otherwise = fixpoint f fx
  where
    fx = f x


-- This takes 2 predicates isFinished and isOK
-- and a start value and a next function
-- it returns True if isOK happens before isFinished
race :: (a -> Bool) -> (a -> Bool) -> a -> (a -> a) -> Bool
race isFinished isOk x next 
  | isFinished nxt = False
  | isOk nxt = True
  | otherwise = race isFinished isOk nxt next
  where
    nxt = next x



------------------- BOOLEAN / BINARY TO/FROM INT

-- This does conversion units at the front of the list
toInt :: [Bool] -> Integer
toInt [] = 0
toInt bs = 2 * toInt (tail bs) + if head bs then 1 else 0


fromInt :: Integer -> [Bool]
fromInt 0 = [False]
fromInt i = helper i
  where
    helper 0 = []
    helper i = let (q,r) = i `divMod` 2 in (r==1) : helper q


pad :: Integer -> a -> [a] -> [a]
pad n b bs = replicate (fromIntegral n - length bs) b ++ take (fromIntegral n) bs


------------------------ SPLITTING OF STRINGS -----------------------



-- Like words but you specify the character
splitOnChar :: Eq a => a -> [a] -> [[a]]
splitOnChar c = reverse . go []
  where
    go acc [] = acc
    go [] (x:xs)
      | x==c = go [] xs
      | otherwise = go [[x]] xs
    go acc@(w:ws) (x:xs)
      | x==c = go ([]:acc) xs
      | otherwise = go ((w++[x]):ws) xs


--splitOnStr :: Eq a => [a] -> [a] -> [[a]]
--splitOnStr = splitOn


------------------------ COORDINATE / VECTOR STUFF ------------

-- Traditionally measured down from top left
-- Coords are (x, y)

type Coord = (Int, Int)


instance Num Coord where
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, 0)


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) +  abs (y1 - y2)

euclidian :: Coord -> Coord -> Double
euclidian (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


neighbours :: Coord -> [Coord]
neighbours c = neighbourCoords `at` c


at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (+ origin) coords


mul :: Int -> Coord -> Coord
mul c (x,y) = (c*x, c*y)


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concat $ (\c -> (c,) <$> [0..(rows-1)]) <$> [0..(cols-1)]



directions :: [Coord]
directions = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]


-- Coordinate start at top left, so y goes up as you go down
leftOf, rightOf, above, below :: Coord -> Coord
leftOf x = x + (-1,0)
rightOf x = x + (1,0)
above x = x + (0,-1)
below x = x + (0,1)
