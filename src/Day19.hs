{-# LANGUAGE FlexibleInstances #-}

module Day19 where


import Utils hiding (allCoords, Coord)
import qualified Data.Map.Strict as M
import qualified Data.Set as S


paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]


type Coord = (Integer, Integer, Integer)

type Dist = Integer 
type Index = Int

magnitude :: Coord -> Dist
magnitude (x,y,z) = (x*x)+(y*y)+(z*z)
--magnitude (x,y,z) = abs x + abs y + abs z

--instance {-# OVERLAPS #-} Eq Coord where
--  v == w = magnitude v == magnitude w


instance Num Coord where
  (x,y,z) + (l,m,n) = (x+l,y+m,z+n)
  (x,y,z) - (l,m,n) = (x-l,y-m,z-n)
  (x,y,z) * (l,m,n) = (x*l,y*m,z*n)
  abs (x,y,z) = (abs x,abs y,abs z)
  signum v = undefined 
  fromInteger = undefined


type Scanner = [Coord]


parseScanner :: [String]  -> [Coord]
parseScanner s = go [] $ tail s
  where
    go acc [] = acc
    go acc (l:ls) = if l=="" then acc else go (v:acc) ls
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
        
t2l :: (a, a) -> [a]
t2l (x,y) = [x,y]


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


findMapping :: Scanner -> Scanner -> (Map Int Int, Coord->Coord)
--findMapping :: Scanner -> Scanner -> Map Index Index
findMapping s1 s2 = (mp, findTransformation)
  where
    b1 = beaconDists s1
    b2 = beaconDists s2
    mp = M.fromList $ (\ix1 -> (ix1, findIndex ix1)) <$> ixs1
    similar = M.elems $ getSimilar b1 b2
    ixs1 = nub $ concat $ t2l . fst <$> similar
    ixs2 = nub $ concat $ t2l . snd <$> similar
    
    allPossibles = t2l . snd <$> concat ((\ix1 -> filter (\((x,y),_) -> x==ix1 || y == ix1) similar) <$> ixs1)
    findIndex :: Index -> Index
    findIndex ix = head $ foldl1 intersect $ t2l . snd <$> filter (\((x,y),_) -> x==ix || y==ix) similar

    findTransformation :: Coord -> Coord
    findTransformation = head ts
      where
        ts :: [Coord -> Coord]
        ts = do
          t <- transformations
          guard (allTheSame $ (\(ix, iy) -> s1 !! ix - t (s2 !! iy) ) <$> M.toList mp)
          return t


allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

findPos :: [Coord] -> [Coord] ->  Coord
findPos beacons1 beacons2 = undefined
  where
    trans = go Nothing transformations
    go acc [] = acc
    go acc (t:ts) = undefined
      where
        s12 = zipWith (\b1 b2 -> b1 - t b2) beacons1 beacons2


--findAllMappings :: [Scanner] -> [((Index, Index), (Map Index Index, Coord -> Coord))]
findAllMappings scanners = go [] $ zip [0..] scanners
--findAllMappings scanners = [(n1, p1), (n2, p2)]
  where
    go acc [] = acc
    go acc ((ix,x):ys) = go (acc ++ ((\(iy,y) -> ((ix, iy), findMapping x y)) <$> ys)) ys

    allMappings = go [] $ zip [0..] scanners

    beaconMaps :: [((Index, Index), Map Index Index)] 
    beaconMaps = (\(x,(y,z)) -> (x,y)) <$> filter (\(_, (mp,_)) -> M.size mp >=12) allMappings
    transforms :: [Coord -> Coord] 
    transforms = (\(x,(y,z)) -> z) <$> filter (\(_, (mp,_)) -> M.size mp >=12) allMappings

    ((i,j), mp) = head beaconMaps

    (n1, t1, p1) = go' (0, id)
    (n2, t2, p2) = go' (n1, t1)

    go' (currentScanner, currentTransform ) = (nextScanner, currentTransform . nextTransform, pos)
      where
        ((_, nextScanner), lmp) = head $ filter (\((x,_),_)-> x==currentScanner) beaconMaps
        currentIx = head $ fst <$> M.toList lmp
        nextIx = head $ snd <$> M.toList lmp
        nextTransform = transforms!!currentIx
        pos = scanners!!currentScanner!!currentIx - currentTransform (nextTransform (scanners!!nextScanner!!nextIx))




day19 :: IO ()
day19 = do
  --ls <- getLines 19
  ls <- getTest 19
  let ps = paragraphs ls
      scanners = parseScanner <$> ps
      num = sum $ length <$> scanners
      beaconMaps = (\(x,(y,z)) -> (x,y)) <$> filter (\(_, (mp,_)) -> M.size mp >=12) (findAllMappings scanners)
      transforms = (\(x,(y,z)) -> z) <$> filter (\(_, (mp,_)) -> M.size mp >=12) (findAllMappings scanners)

      ((i,j), mp) = head beaconMaps
      ixs, jxs :: [Index]
      ixs = fst <$> M.toList mp
      jxs = snd <$> M.toList mp
      --x :: Index
      --x = 1
      ppp = scanners!!i!!(ixs!!0) - (transforms!!i) (scanners!!j!!(jxs!!0))
      pos = zipWith (\ix jx -> scanners!!i!!ix - (transforms!!i) (scanners!!j!!jx)) ixs jxs



  --putStrLn $ "Day19: part1:mp " ++ show (second M.size <$> beaconMaps)
  --putStrLn $ "Day19: part1:mp " ++ show (sort $ head transforms <$> scanners !! 1)
  --putStrLn $ "Day19: part1:mp " ++ show (sort $ scanners !! 0)
  putStrLn $ "Day19: part1:mp " ++ show (fst <$> beaconMaps)
  putStrLn $ "Day19: part1:mp " ++ show (ppp)
  --putStrLn $ "Day19: part1:mp " ++ show (findAllMappings scanners)
  
  
  
  
  

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