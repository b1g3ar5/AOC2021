
module Day5 where


import Utils (getLines, splitOn, Coord)
import Data.List (sort, group)


type Line = (Coord, Coord)


toCoord :: Line -> [Coord]
toCoord ((x1, y1), (x2, y2))
  | x1==x2 = [(x1, y) | y <- [y1, y1 + signum (y2 - y1)..y2]]
  | y1==y2 = [(x, y1) | x <- [x1, x1 + signum (x2 - x1)..x2]]
  | otherwise = [(x, y1 `op` (x-x1)) | x <- [x1, x1 + signum (x2 - x1)..x2]]
  where
    op = if ((x2-x1)*(y2-y1))>0 then (+) else (-)


isHorizontal :: Line -> Bool
isHorizontal ((_,y1), (_,y2)) = y1==y2


isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1==x2


isOrthogonal :: Line -> Bool
isOrthogonal l = isVertical l || isHorizontal l


parseLine :: String -> Line
parseLine s = ((read $ head p1, read $ p1!!1), (read $ head p2, read $ p2!!1))
  where
    ws = words s
    p1 = splitOn "," $ head ws
    p2 = splitOn "," $ ws!!2


findIntersections :: [Line] -> Int
findIntersections ls = length $ filter (\g -> length g > 1) $ group $ sort $ concat $ toCoord <$> ls



day5 :: IO ()
day5 = do
  inLines <- getLines 5
  --let inLines = test
  let ls = parseLine <$> inLines
      os = filter isOrthogonal ls

  putStrLn $ "Day5: part1: " ++ show (findIntersections os) 
  putStrLn $ "Day5: part1: " ++ show (findIntersections ls) 
  return ()

