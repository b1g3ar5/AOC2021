module Day15b where


import Prelude hiding (lookup)
import Utils hiding (Empty)
import qualified Data.Map.Strict as M
import Algorithm.Search

import Debug.Trace


type Grid = Map Coord Int


parseGrid :: [String] -> Grid
parseGrid ess = M.fromList $ concat $ (\(y, es) -> (\(x, e) -> ((x,y), read [e])) <$> zip [0..(gridSize-1)] es) <$> zip [0..(gridSize-1)] ess


floodFill :: Grid -> Grid
floodFill g = foldl go (M.singleton (0,0) 1) [1..(gridSize-1)]
  where
    go mg level = mg3
      where
        mg1 = M.insert (level,0) (g M.! (level, 0) + mg M.! (level-1,0)) 
            $ M.insert (0, level) (g M.! (0, level) + mg M.! (0, level-1)) mg
        mg2 = foldl (\acc p@(x,y) 
                      -> M.insert p (g M.! p + min (acc M.! (x, y-1)) (acc M.! (x-1, y))) acc
                    ) mg1 $ (level,) <$> [1..(level-1)]
        mg3 = foldl (\acc p@(x,y) 
                      -> M.insert p (g M.! p + min (acc M.! (x, y-1)) (acc M.! (x-1, y))) acc 
                    ) mg2 $ (,level) <$> [1..level]


lookup :: Grid -> Coord -> Int
lookup g (x, y) = 1 + mod (base + qx + qy -1) 9
  where
    (qx, rx) = divMod x 100
    (qy, ry) = divMod y 100
    base = fromMaybe (error $ "error in lookup at (x,y): (" ++ show x ++ ", " ++ show y ++ ")") $ g M.!? (rx, ry)


-- Lookup with big default
lu :: Map Coord Int -> Coord -> Int
lu mp x = fromMaybe 1000000 $ mp M.!? x


start :: (Int, Int)
start = (0,0)
target1, target2 :: (Int, Int)
target1 = (gridSize-1, gridSize-1)
target2 = (5*gridSize - 1, 5*gridSize - 1)
gridSize :: Int
gridSize = 100


type State = (Grid, Coord, [Coord], Int)


inBounds2 :: Coord -> Bool
inBounds2 (x,y) = x>=0 && y>=0 && x<=5*gridSize-1 && y<=5*gridSize-1


moves :: State -> [State]
moves (g,p,vs,n)= trace (show p) $ (\n -> (g, n, p:vs, g M.! n)) <$> ns
  where
    ns = filter inBounds2 $ filter (`notElem` vs) $ neighbours4 p


cost :: State -> State -> Int
cost (_,_,_,c1) (_,_,_,c2)= c2-c1

finished :: State -> Bool
finished (_,p,_,_) = p==target2


solve :: Grid -> Maybe (Int, [State])
solve g = dijkstra moves cost finished (g, (0,0), [], 0)

day15 :: IO ()
day15 = do
  ls <- getLines 15
  --let ls = test
  let grid = parseGrid ls
      mg1 = floodFill grid
      mg2 = floodFill2 grid

  putStrLn $ "Day12:part1: " ++ show (mg1 M.! target1 - 1)
  putStrLn $ "Day12:part2: " ++ show (fst $ fromMaybe (-99, []) $ solve grid) -- 2186 too low, 2882 too high


  return ()




test :: [String]
test = [
    "1163751742"
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


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show


bftree :: [a] -> Tree a
bftree xs = t
    where
    t : q  =  go xs q
    go []       _              =  repeat Empty
    go (x:ys) ~(l : ~(r : q))  =  Node x l r : go ys q


-- This gives the wrong answer - so there must be some left or up turns...
floodFill2 :: Grid -> Grid
floodFill2 g = foldl go (M.singleton (0,0) 1) [1..((5*gridSize) - 1)]
  where
    go mg level = mg3
      where
        mg1 = M.insert (level,0) (g `lookup` (level, 0) + mg M.! (level-1,0)) 
            $ M.insert (0, level) (g `lookup` (0, level) + mg M.! (0, level-1)) mg
        mg2 = foldl (\acc p@(x,y) 
                      -> M.insert p (g `lookup` p + min (acc M.! (x, y-1)) (acc M.! (x-1, y))) acc
                    ) mg1 $ (level,) <$> [1..(level-1)]
        mg3 = foldl (\acc p@(x,y) 
                      -> M.insert p (g `lookup` p + min (acc M.! (x, y-1)) (acc M.! (x-1, y))) acc 
                    ) mg2 $ (,level) <$> [1..level]



coord2int :: Coord -> Int
coord2int (x,y) = y*gridSize+x
int2coord :: Int -> Coord
int2coord i = (x,y)
  where
    (y,x) = quotRem i gridSize



data SkewHeap a = EmptyHeap | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show)


(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2) 
  | x1 <= x2    = SkewNode x1 (heap2 +++ r1) l1 
  | otherwise = SkewNode x2 (heap1 +++ r2) l2
EmptyHeap +++ heap = heap
heap +++ EmptyHeap = heap


extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin EmptyHeap = Nothing
extractMin (SkewNode x l r ) = Just (x , l +++ r )


fromList :: Ord a => [a] -> SkewHeap a
fromList xs = foldl (+++) EmptyHeap nodes
  where nodes = map (\x-> SkewNode x EmptyHeap EmptyHeap) xs


{-
gridSize :: Int
test :: [String]
test = [
    "1999999"
  , "1911199"
  , "1119199"
  , "9999199"
  , "9991199"
  , "9991999"
  , "9991111"
  ]

-}

{-

newtype Fix f = Fix { unFix :: f (Fix f) } 
type Algebra f a = f a -> a
type Coalgebra f a = (a -> f a)
type CVCoalgebra f a = a -> f (Either (Fix f) a)


cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

apo :: Functor f => CVCoalgebra f a -> a -> Fix f
apo f = Fix . fmap (either id (apo f)) . f


-- The seed is (currentPosition, visited)
type Seed = (Int, Coord, [Coord])
data TreeF a r = TNode a [r] deriving (Functor, Eq, Show)
type Tree a = Fix (TreeF a)


mkTree :: Grid -> Tree Coord
mkTree g = ana makeCoalg (20, (0, 0), [])


makeCoalg :: Seed -> TreeF Coord Seed
makeCoalg (n, this, visited)
  | n == 0 = TNode this [] -- Terminate early
  | this == target1 = TNode this [] -- Correct termination
  | target1 `elem` kids = TNode this [(n-1, target1, this:visited)] -- If target is kid
  | otherwise = TNode this $ [(n-1, k, this:visited) | k <- kids]
  where
    visitAllowed c = c `notElem` visited && inBounds c
    kids = filter visitAllowed $ neighbours4 this


render :: Tree Coord  -> [String]
render = cata renderAlg
renderAlg :: TreeF Coord [String] -> [String]
renderAlg (TNode n []) = [show n]
renderAlg (TNode n ss) = (\s -> show n ++ ", " ++ s) <$> concat ss


minPath :: Grid -> Tree Coord -> Int
minPath g = cata (pathAlg g)
pathAlg :: Grid -> TreeF Coord Int -> Int
pathAlg g (TNode n ss)
  | n == target1 = g M.! n -- If we're at the target just return the cost
  | null ss = 1000000 -- If not at the target and nowhere to go PENALTY
  | otherwise = g M.! n + minimum ss

-}


