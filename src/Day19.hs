{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module Day19 where


import Utils (Map, fromJust, isNothing, (\\), nub, sort, getLines, splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Bimap as B
import qualified Data.Set as S
import Frames (applyFrame, findFrame, inverse, Coord, Frame(origin), manhattan)


-- Such a complicated puzzle that I had to write a library for it... Frames.hs


parseScanner :: [String]  -> [Coord]
parseScanner s = go [] $ tail s
  where
    go acc [] = acc
    go acc (l:ls) = if l=="" then acc else go (acc ++ [v]) ls
      where
        cs = splitOn "," l
        v = (read $ head cs, read $ cs!!1, read $ cs!!2)


paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]


type Index = Int
type Scanner = [Coord]

      
type Frames = Map (Int, Int) Frame


findFrames :: [Scanner] -> Frames
findFrames scanners = go M.empty $ zip [0..] scanners
  where
    go :: Frames -> [(Int, Scanner)] -> Frames
    go acc [] = acc
    go acc [x] = acc
    go acc ((ix,xcs):ys) = go (foldl gg acc ys) ys
      where
        gg mp (iy, ycs)
          | isNothing ff = mp
          | otherwise = M.insert (iy, ix) (inverse $ fromJust ff) $ M.insert (ix, iy) (fromJust ff) mp
          where
            ff = findFrame xcs ycs


zeroFrames :: Frames -> Map Int Frame
zeroFrames mp = go M.empty $ ixs \\ [0]
  where
    ixs = nub $ concat $ (\(i,j) -> [i,j]) <$> M.keys mp
    go :: Map Int Frame -> [Int] -> Map Int Frame
    go acc [] = acc
    go acc (iy:iys)
      | isNothing f && null ks = go acc (iys ++ [iy]) -- put it at the end!!
      | isNothing f = go (M.insert iy ((acc M.! head ks) <> g) acc) iys
      | otherwise = go (M.insert iy (fromJust f) acc) iys
      where
        f = mp M.!? (0, iy)
        doneKeys = M.keys acc
        ks = filter (`elem` doneKeys) $ fst <$> M.keys (M.filterWithKey (\(_,y) _ -> y == iy) mp)
        g = mp M.! (head ks, iy)


maxDist :: [Coord] -> Int 
maxDist = go 0
  where
    go n [] = n
    go n [x] = n
    go n (x:zs) = go (foldl (\m z -> max m $ manhattan (x - z)) n zs) zs


day19 :: IO ()
day19 = do
  ls <- getLines 19
  let ps = paragraphs ls
      scanners = parseScanner <$> ps
      coframes = findFrames scanners
      frames0 = zeroFrames coframes
      allCoords = sort $ nub $ concat $ scanners !!0 : ((\(i,f) -> applyFrame f <$> scanners !! i) <$> M.toList frames0)

      
  putStrLn $ "Day19: part1: " ++ show (length allCoords)
  putStrLn $ "Day19: part2: " ++ show (maxDist $ origin <$> M.elems frames0) 

  return ()


