{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Utils (
  getLines
  , getTest
  , getRaw
  , getWords
  , splitOn
  , toInt
  , fromInt
  , pad
  , Coord
  , allCoords
  , neighbourCoords4
  , neighbourCoords8
  , splitAt
  , intersections
  , clockTurn
  , antiTurn
  , directions8
  , directions4
  , leftOf
  , rightOf
  , above
  , below
  , neighbours4
  , neighbours8
  , span
  , race
  , chunksOf
  , first
  , second
  , bimap
  , fromMaybe
  , catMaybes
  , sort
  , sortOn
  , transpose
  , steadyState
  , steadyState'
  , swap
  , (\\)
  , toLower
  , toUpper
  , isAsciiLower
  , isAsciiUpper
  , intercalate 
  , nub
  , group
  , M.Map(..)
  , S.Set(..)
  , Seq(..)
  , IM.IntMap(..)
  , V.Vector(..)
  , guard
  , fromJust
  , isJust
  , intersect
  , isNothing
  , safeHead
  , delete
  , replace
  , trace
  , Semigroup(..)
  , Monoid(..)
  , bool
  , fix
  , timeIt
)
where


import qualified Data.Set as S
--import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq(..), (><), (|>), (<|))
import Data.List ( foldl', transpose, (\\), delete, group, intercalate, intersect, nub, sort, sortOn )
import Data.List.Split (chunksOf, splitOn)
import Data.List.Utils (replace)
import Data.Bifunctor (Bifunctor(bimap, first, second))
import Data.Tuple (swap)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Char (isAsciiLower, isAsciiUpper, toLower, toUpper)
import Control.Monad (guard)
import Control.Monad.ST (runST, ST(..))
import System.TimeIt ( timeIt )
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Debug.Trace (trace)
import Data.Bool

--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

------------ GET THE INPUT FROM FILE ------------------

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".in"
  return $ f s

getT :: (String -> a) -> Int -> IO a
getT f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".ex"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getTest :: Int -> IO [String]
getTest = getT lines

------------------ VARIOUS UTILITY FUNCTIONS --------------------


intersections :: Ord a => [S.Set a] -> S.Set a
intersections ss = foldl' S.intersection (head ss) (tail ss)


fix :: Eq a => (a -> a) -> a
fix f = x where x = f x


steadyState :: Eq a => (a -> a) -> a -> a
steadyState f x = if f x == x then x else steadyState f (f x)


steadyState' :: Eq a => (a -> Maybe a) -> a -> Maybe a
steadyState' f x = case f x of
                     Nothing -> Just x
                     Just y -> steadyState' f y


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



-- Like words but you specify the character - very slow, I expect
splitOnChar :: Char -> String -> [String]
splitOnChar c s = (go <$>) <$> words tc
  where
    -- turn c into space and space into c
    go :: Char -> Char
    go x
      | x == c = ' '
      | x == ' ' = c
      | otherwise = x
    tc = go <$> s


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


largePrime = 11939


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) +  abs (y1 - y2)


euclidian :: Coord -> Coord -> Double
euclidian (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


neighbourCoords4 :: [Coord]
neighbourCoords4 = [(1,0), (-1,0), (0,1), (0,-1)]


neighbourCoords8 :: [Coord]
neighbourCoords8 = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


neighbours4 :: Coord -> [Coord]
neighbours4 c = neighbourCoords4 `at` c


neighbours8 :: Coord -> [Coord]
neighbours8 c = neighbourCoords8 `at` c



at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (+ origin) coords


mul :: Int -> Coord -> Coord
mul c (x,y) = (c*x, c*y)


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concat $ (\c -> (c,) <$> [0..(rows-1)]) <$> [0..(cols-1)]


directions8 :: [Coord]
directions8 = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]

directions4 :: [Coord]
directions4 = [(0, -1), (0, 1), (1, 0), (-1, 0)]


-- Coordinate start at top left, so y goes up as you go down
leftOf, rightOf, above, below :: Coord -> Coord
leftOf x = x + (-1,0)
rightOf x = x + (1,0)
above x = x + (0,-1)
below x = x + (0,1)


safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (x:_) = Just x

