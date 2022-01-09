{-# language FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}

module Day25 where


import Utils hiding (Empty)
import qualified Data.Map.Strict as M


-- LIST VERSION - simple but very slow...

isCuc, notCuc, isDn, isRt :: Char -> Bool
isCuc c = c `elem` ">v"
notCuc = not . isCuc
isDn = (=='v')
isRt = (=='>')


move :: Char -> String -> String
move !cuc !cs = go [] cs
  where
    h = head cs
    go :: String -> String -> String
    go acc [] = acc
    go (c:cs) [l]
      | notCuc h && l==cuc = l:cs ++ [h]
      | otherwise = c:cs ++ [l]
    go acc (x : y : ys)
      | x==cuc && isCuc y = go (acc ++ [cuc]) (y:ys)
      | x==cuc = go (acc ++ ['.',cuc]) ys
    go acc (y:ys) = go (acc ++ [y]) ys


moveSouth, moveEast :: String ->String
moveSouth = move 'v'
moveEast = move '>'


loop :: [String] -> [String]
loop ls = transpose $ moveSouth <$> transpose (moveEast <$> ls)


steadyStateWithCount :: Eq a => Int -> (a -> a) -> a -> (Int, a)
steadyStateWithCount n f x = if fx == x then (n+1,x) else steadyStateWithCount (n+1) f fx
  where
    fx = f x


day25 :: IO ()
day25 = do
  ls <- getLines 25
  --let ls = test
  let grid = parse ls
  putStrLn $ "Day25: part1:Map\n" ++ show (fst $ steadyStateWithCount 0 update grid)

  return ()


-- MAP VERSION - will be quicker

data Cell = Empty | Righter | Downer deriving (Eq, Show)
type Grid = Map Coord Cell


showCell :: Cell -> Char
showCell Empty = '.'
showCell Righter = '>'
showCell Downer = 'v'


parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell '>' = Righter
parseCell 'v' = Downer
parseCell c = error $ "This is not a fish: " ++ [c]


parse :: [String] -> Map Coord Cell
parse css = M.fromList $ concat $ (\(y, cs) -> (\(x, c) -> ((x,y), parseCell c)) <$> zip [0..] cs) <$> zip [0..] css


width, height :: Int
width = 139
height = 137


lt, dn, rt, up :: Coord -> Coord
lt (x,y) = ((x-1) `mod` width, y)
dn (x,y) = (x, (y+1) `mod` height)
rt (x,y) = ((x+1) `mod` width, y)
up (x,y) = (x, (y-1) `mod` height)


update :: Grid -> Grid
update mp = downMoves `M.union` rightMap
  where
    rightMovable = M.filterWithKey (\k c -> c == Righter && (mp M.! rt k) == Empty) mp
    rightMoves = M.fromList $ concat $ M.mapWithKey (\k c -> [(k, Empty), (rt k, Righter)]) rightMovable
    rightMap = rightMoves `M.union` mp
    downMovable = M.filterWithKey (\k c -> c == Downer && (rightMap M.! dn k) == Empty) rightMap
    downMoves = M.fromList $ concat $ M.mapWithKey (\k c -> [(k, Empty), (dn k, Downer)]) downMovable


render :: Grid -> String
render mp = unlines $ (\y -> (\x -> showCell $ mp M.! (x,y)) <$> [0..(width-1)]) <$> [0..(height-1)]


