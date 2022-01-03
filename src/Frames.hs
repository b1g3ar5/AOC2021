{-# LANGUAGE FlexibleInstances  #-}

module Frames where

import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.Bimap as B
import Data.Maybe (fromJust, isJust)
import Data.Bifunctor (Bifunctor(second))
import Data.List (nub, intersect, transpose)


type Dist = Int
type Coord = (Int, Int, Int)
type Rotation = Coord -> Coord
type Matrix = [[Int]]
data Frame = Frame { origin :: Coord
                  , rotation :: Rotation
                  } 
type DistMap = IntMap (Int, Int)


instance {-# Overlapping #-} Semigroup Rotation where
  r1 <> r2 = r1 . r2

instance {-# Overlapping #-} Monoid Rotation where
  mempty = id

instance Semigroup Frame where
  (Frame o1 t1) <> (Frame o2 t2) = Frame (o1 + t1 o2) (t1 <> t2)

instance Monoid Frame where
  mempty = Frame (0,0,0) mempty

inverse :: Frame -> Frame
inverse (Frame o r) = Frame (- inv r o) (inv r)

instance Show Frame where
  show (Frame o r) = show o ++ ": " ++ show (r (1,2,3))


manhattan :: Coord -> Int 
manhattan (x,y,z) = abs x + abs y + abs z


mmult :: Matrix -> (Coord -> Coord)
mmult [[v11, v12, v13], [v21, v22, v23], [v31, v32, v33]]  = \(x,y,z) -> (x*v11+y*v12+z*v13, x*v21+y*v22+z*v23, x*v31+y*v32+z*v33)
mmult _ = error "Matrix error in mmult - the vectors must have 3 elements..."


toMatrix :: Rotation -> Matrix
toMatrix f = (\i -> if abs i == 1 
                    then [signum i,0,0]
                    else if abs i == 2 
                         then [0, signum i, 0]
                         else [0,0,signum i]
              ) <$> ixs
  where
    ixs = (\(x,y,z) -> [x,y,z]) $ f (1,2,3)


fromMatrix :: Matrix -> Rotation
fromMatrix = mmult


-- Inverse rotation by matrix transpose
inv :: Rotation -> Rotation
inv r = fromMatrix $ transpose $ toMatrix r


tuple2list :: (a, a) -> [a]
tuple2list (x,y) = [x,y]


-- Converts coords in the second frame to that of the first frame
applyFrame :: Frame -> Coord -> Coord
applyFrame (Frame ori rot) pos = rot pos + ori


magnitude :: Coord -> Dist
magnitude (x,y,z) = (x*x)+(y*y)+(z*z)


instance Num Coord where
  (x,y,z) + (l,m,n) = (x+l,y+m,z+n)
  (x,y,z) - (l,m,n) = (x-l,y-m,z-n)
  (x,y,z) * (l,m,n) = (x*l,y*m,z*n)
  abs (x,y,z) = (abs x,abs y,abs z)
  signum v = error $ "signum called with: " ++ show v
  fromInteger 0 = (0,0,0)
  fromInteger 1 = (1,0,0)
  fromInteger v = error $ "fromInteger called with: " ++ show v


transformations :: Num a => [(a, a, a) -> (a, a, a)]
transformations = do
    o <- orders
    p <- perms
    return (o . p)
  where
    perms, orders :: Num a => [(a,a,a) -> (a,a,a)]
    perms = [ \(x,y,z)->(x,y,z)
            , \(x,y,z)->(-x,y,z)
            , \(x,y,z)->(x,-y,z)
            , \(x,y,z)->(x,y,-z)
            , \(x,y,z)->(-x,-y,z)
            , \(x,y,z)->(-x,y,-z)
            , \(x,y,z)->(x,-y,-z)
            , \(x,y,z)->(-x,-y,-z)]
    orders = [\(x,y,z)->(x,y,z), \(x,y,z)->(y,z,x), \(x,y,z)->(z,x,y)
             ,\(x,y,z)->(x,z,y), \(x,y,z)->(y,x,z), \(x,y,z)->(z,y,x)]


-- The components are  column vectors
mats :: [(Coord, Coord, Coord)]
mats = [x0, x1, x2, x3, y1, y2, y3, z1, z2, z3] 
  where
    x0 = ((1,0,0),(0,1,0),(0,0,1))
    x1 = ((1,0,0),(0,0,1),(0,-1,0))
    x2 = ((1,0,0),(0,-1,0),(0,0,-1))
    x3 = ((1,0,0),(0,0,-1),(0,1,0))
    y1 = ((0,0,-1),(0,1,0),(1,0,0))
    y2 = ((-1,0,0),(0,1,0),(0,0,-1))
    y3 = ((0,0,1),(0,1,0),(-1,0,0))
    z1 = ((0,-1,0),(1,0,0),(0,0,1))
    z2 = ((-1,0,0),(0,-1,0),(0,0,1))
    z3 = ((0,1,0),(-1,0,0),(0,0,1))


findFrame :: [Coord] -> [Coord] -> Maybe Frame
findFrame cs1 cs2
  | length overlap < 12 = Nothing -- We need 12 similar distances
  | null ts = Nothing  -- No transformation works
  | otherwise = Just $ Frame pos $ head ts
  where
    dmap1 = mkMap cs1
    dmap2 = mkMap cs2
    overlap = I.elems $ I.intersectionWith (,) dmap1 dmap2
    ixs1 = nub $ concat $ tuple2list . fst <$> overlap
    ixs2 = nub $ concat $ tuple2list . snd <$> overlap
    mp = B.fromList $ second fromJust <$> filter (\(_, mi) -> isJust mi) ((\ix1 -> (ix1, findIndex ix1)) <$> ixs1)
    -- Transforms that work for all 12 beacons
    ts :: [Coord -> Coord]
    ts = do
      t <- transformations
      guard (allEq $ (\(ix, iy) -> cs1 !! ix - t (cs2 !! iy) ) <$> B.toList mp)
      return t
    -- Apply the working transform to get the relative position of scanner 2
    pos = cs1!!i1 - head ts (cs2!!i2)
    (i1, i2) = head $ B.toList mp
    
    -- An index of the second scanner that might be equivalent to ix
    findIndex :: Int -> Maybe Int
    findIndex ix = safeHead $ foldl1 intersect $ tuple2list . snd <$> filter (\((x,y),_) -> x==ix || y==ix) overlap


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq xs = (==1) . length . nub $ xs


-- maps distance to the indexes of the coords that are that distance apart
mkMap :: [Coord] -> DistMap
mkMap s = go I.empty $ zip  [0..] s
  where
    go :: DistMap -> [(Int, Coord)] -> DistMap
    go acc [] = acc
    go acc [x] = acc
    go acc ((ix,x):ys) = go (I.union acc $ I.fromList ds) ys
      where
        ds :: [(Dist, (Int, Int))]
        ds = (\(iy, y) -> (magnitude (y-x), (ix, iy))) <$> ys


 
