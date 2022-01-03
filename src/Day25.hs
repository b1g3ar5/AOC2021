module Day25 where


import Utils hiding (Empty)
import qualified Data.Map.Strict as M

-- Simple but very slow...

isCuc, notCuc :: Char -> Bool
isCuc c = c `elem` ">v"
notCuc = not . isCuc

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
  let grid = parse ls
  --putStrLn $ "Day25: part1:\n" ++ show (fst $ steadyStateWithCount 0 loop ls)
  putStrLn $ "Day25: part1:\n" ++ show (fst $ steadyStateWithCount 0 update grid)

  return ()


-- Maps might be a bit quicker?

data Cell = Empty | Lefter | Downer deriving (Eq, Show)
type Grid = Map Coord Cell


showCell :: Cell -> Char
showCell Empty = '.'
showCell Lefter = '>'
showCell Downer = 'v'

parseCell '.' = Empty
parseCell '>' = Lefter
parseCell 'v' = Downer
parseCell c = error $ "This is not a fish: " ++ [c]


parse :: [String] -> Map Coord Cell
parse css = M.fromList $ concat $ (\(y, cs) -> (\(x, c) -> ((x,y), parseCell c)) <$> zip [0..] cs) <$> zip [0..] css


width, height :: Int
width = 139
height = 137


left, down :: Coord -> Coord
left (x,y) = ((x+1) `mod` width, y)
down (x,y) = (x, (y+1) `mod` height)


update :: Grid -> Grid
update mp = downMap `M.union` mp1
  where
    leftMoves = M.filterWithKey (\k c -> c == Lefter && (mp M.! left k) == Empty) mp
    leftMap = M.fromList $ concat $ M.mapWithKey (\k c -> [(k, Empty), (left k, Lefter)]) leftMoves
    mp1 = leftMap `M.union` mp
    downMoves = M.filterWithKey (\k c -> c == Downer && (mp1 M.! down k) == Empty) mp1
    downMap = M.fromList $ concat $ M.mapWithKey (\k c -> [(k, Empty), (down k, Downer)]) downMoves


render :: Grid -> String
render mp = unlines $ (\y -> (\x -> showCell $ mp M.! (x,y)) <$> [0..(width-1)]) <$> [0..(height-1)]

