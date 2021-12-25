{-# LANGUAGE FlexibleInstances #-}

module Day19 where


import Utils hiding (allCoords, Coord)
import qualified Data.Map.Strict as M
import qualified Data.Bimap as B
import qualified Data.Set as S

import Debug.Trace

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]


type Coord = (Integer, Integer, Integer)

type Dist = Integer 
type Index = Int

magnitude :: Coord -> Dist
magnitude (x,y,z) = (x*x)+(y*y)+(z*z)


instance Num Coord where
  (x,y,z) + (l,m,n) = (x+l,y+m,z+n)
  (x,y,z) - (l,m,n) = (x-l,y-m,z-n)
  (x,y,z) * (l,m,n) = (x*l,y*m,z*n)
  abs (x,y,z) = (abs x,abs y,abs z)
  signum v = undefined 
  fromInteger = undefined


type Scanner = [Coord]


data Transformation = Transformation {
    s1 :: Int
  , s2 :: Int
  , beacons :: B.Bimap Int Int
  , transformation :: Maybe (Coord -> Coord)
  , position :: Maybe Coord
} 

instance Show Transformation where
  show (Transformation s1 s2 bs t p) = "s1: " ++ show s1 ++ ", s2: " ++ show s2 ++ ", beacons: " ++ show bs ++ ", position: " ++ show p

parseScanner :: [String]  -> [Coord]
parseScanner s = go [] $ tail s
  where
    go acc [] = acc
    go acc (l:ls) = if l=="" then acc else go (acc ++ [v]) ls
      where
        cs = splitOn "," l
        v = (read $ head cs, read $ cs!!1, read $ cs!!2)


-- A map from the distance between 2 beacons to the indecies of the beacons
type BeaconMap = Map Dist (Index, Index)


beaconDists :: [Coord] -> BeaconMap
beaconDists s = go M.empty $ zip  [0..] s
  where
    go :: Map Dist (Index, Index) -> [(Index, Coord)] -> Map Dist (Index, Index)
    go acc [] = acc
    go acc [x] = acc
    go acc ((ix,x):ys) = go (M.union acc $ M.fromList ds) ys
      where
        ds :: [(Dist, (Index, Index))]
        ds = (\(iy, y) -> (magnitude (y-x), (ix, iy))) <$> ys
  

-- Gets the pair of beacobs which have the same distance for 2 scanners
getSimilar :: BeaconMap -> BeaconMap -> Map Dist ((Index, Index),(Index, Index))
getSimilar = M.intersectionWithKey (\k v w -> (v,w))
        
toList :: (a, a) -> [a]
toList (x,y) = [x,y]


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


findMapping :: Scanner -> Scanner -> (B.Bimap Int Int, Maybe (Coord->Coord))
findMapping s1 s2 = (mp, findTransformation)
  where
    b1 = beaconDists s1
    b2 = beaconDists s2
    mp = B.fromList $ second fromJust <$> filter (\(_, mi) -> isJust mi) ((\ix1 -> (ix1, findIndex ix1)) <$> ixs1)
    similar = M.elems $ getSimilar b1 b2
    ixs1 = nub $ concat $ toList . fst <$> similar
    ixs2 = nub $ concat $ toList . snd <$> similar
    
    allPossibles = toList . snd <$> concat ((\ix1 -> filter (\((x,y),_) -> x==ix1 || y == ix1) similar) <$> ixs1)
    findIndex :: Index -> Maybe Index
    findIndex ix = safeHead $ foldl1 intersect $ toList . snd <$> filter (\((x,y),_) -> x==ix || y==ix) similar

    findTransformation :: Maybe (Coord -> Coord)
    findTransformation = safeHead ts
      where
        ts :: [Coord -> Coord]
        ts = do
          t <- transformations
          guard (allTheSame $ (\(ix, iy) -> s1 !! ix - t (s2 !! iy) ) <$> B.toList mp)
          return t


allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)



findAllMappings :: [Scanner] -> [Transformation]
findAllMappings scanners = go [] $ zip [0..] scanners
  where
    go :: [Transformation] -> [(Int, Scanner)] -> [Transformation]
    go acc [] = acc
    go acc ((ix,x):ys) = go (acc ++ ((\(iy,y) -> 
      let (mp, tr) = findMapping x y in relPos x y (Transformation ix iy mp tr Nothing)
      ) <$> ys)) ys


relPos :: Scanner -> Scanner -> Transformation -> Transformation
relPos s1 s2 t@(Transformation ix1 ix2 mp transform _)
  | B.null mp = t
  | isNothing transform = t
  | otherwise = Transformation ix1 ix2 mp transform (Just $ s1!!i1 - fromJust transform (s2!!i2))
  where
    i1 = head $ fst <$> B.toList mp
    i2 = head $ snd <$> B.toList mp


blookup :: (Ord a, Ord b) => B.Bimap a b -> a -> Maybe b
blookup mp x = if x `B.member` mp then Just (mp B.! x) else Nothing

blookupR :: (Ord a, Ord b) => B.Bimap a b -> b -> Maybe a
blookupR mp x = if x `B.memberR` mp then Just (mp B.!> x) else Nothing


convertBack :: Transformation -> Maybe (Coord -> Coord)
convertBack t 
  | isJust (position t) = Just $ \c -> fromJust (position t) + fromJust (transformation t) c
  | otherwise = Nothing

-- All t^6 == id for all tranformations, so inverse t = t^5
convertForward :: Transformation -> Maybe (Coord -> Coord)
convertForward t
  | isJust (position t) = Just $ \c -> iterate (fromJust (transformation t)) (c - fromJust (position t)) !! 5
  | otherwise = Nothing


convertBeacons :: [[Coord]] -> [Transformation] -> [Coord]
convertBeacons scanners transformations = concat $ (\(ix, f) -> f <$> scanners !! ix) <$> M.toList conversionFunctions
  where
    conversionFunctions = go (M.fromList [(0, id)]) [1..33] 
    go :: Map Int (Coord -> Coord) -> [Int] -> Map Int (Coord -> Coord)
    go acc [] = acc
    go acc (x:todo)
      | not (null t2) = go (M.insert x ((acc M.! s1 (head t2)) . fromJust (convertBack $ head t2)) acc) todo
      | not (null t1) = go (M.insert x ((acc M.! s2 (head t1)) . fromJust (convertForward $ head t1)) acc) todo
      | otherwise = trace (show $ todo ++ [x]) 
                  go acc (todo ++ [x])

      where
        t1, t2 :: [Transformation]
        t1 = filter (\t -> s2 t `M.member` acc) $ filter (\t -> s1 t == x) transformations
        t2 = filter (\t -> s1 t `M.member` acc) $ filter (\t -> s2 t == x) transformations


hhead :: [a] -> a
hhead [] = error "Head of null list"
hhead (x:_) = x


day19 :: IO ()
day19 = do
  ls <- getLines 19
  --ls <- getTest 19
  let ps = paragraphs ls
      scanners = parseScanner <$> ps
      num = sum $ length <$> scanners

      transformations = findAllMappings scanners
      filteredTransformations = filter (\t -> B.size (beacons t) >= 12) transformations
      firstMapKeys = B.keys $ beacons $ head filteredTransformations
      secondMapKeys = B.keysR $ beacons $ head filteredTransformations

      ts = catMaybes $ transformation <$> filteredTransformations
      scannerPositions = catMaybes $ position <$> filteredTransformations
      

  --putStrLn $ "Day19: part1:mp " ++ show ((\t -> ((s1 t, s2 t), B.size $ beacons t)) <$> filteredTransformations)
  --putStrLn $ "Day19: part1:beacons0 " ++ show (sort $ (\k -> head scanners !! k) <$> firstMapKeys)
  --putStrLn $ "Day19: part1:beacons0 " ++ show (sort $ (\k -> head scannerPositions + head ts (scanners!!1 !! k)) <$> secondMapKeys)

  --putStrLn $ "\nDay19: part1:beacons0 " ++ show (sort $ (\k -> scanners !! 1 !! k) <$> secondMapKeys)
  --putStrLn $ "Day19: part1:beacons0 " ++ show (sort $ (\k -> head ts (head scanners !! k - head scannerPositions)) <$> firstMapKeys)

  --putStrLn $ "Day19: part1:beacons0 " ++ show (head ts (1,2,3))


  --putStrLn $ "Day19: part1:beacon1 " ++ show (scannerPositions!!0)
  --putStrLn $ "Day19: part1:beacon2 " ++ show (scannerPositions!!0 + (ts!!0) (scannerPositions!!2) + (ts!!2) ((ts!!3) (scannerPositions!!3)))
  --putStrLn $ "Day19: part1:beacon3 " ++ show (scannerPositions!!0 + (ts!!0) (scannerPositions!!1))
  --putStrLn $ "Day19: part1:beacon4 " ++ show (scannerPositions!!0 + (ts!!0) (scannerPositions!!2))

  putStrLn $ "Day19: part1:scanners " ++ show (length scanners)
  putStrLn $ "Day19: part1:filteredTransformations " ++ show ((\t -> (s1 t, s2 t)) <$> filteredTransformations)
  putStrLn $ "Day19: part1:beacons " ++ show (length $ nub $ convertBeacons scanners filteredTransformations)

  
  
  
  
  

  return ()


{-

686,422,578
605,423,415
515,917,-361
-336,658,858
-476,619,847
-460,603,-452
729,430,532
-322,571,750
-355,545,-477
413,935,-424
-391,539,-444
553,889,-390

-618,-824,-621
-537,-823,-458
-447,-329,318
404,-588,-901
544,-627,-890
528,-643,409
-661,-816,-575
390,-675,-793
423,-701,434
-345,-311,381
459,-707,401
-485,-357,347
-}