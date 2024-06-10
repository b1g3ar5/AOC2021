{-# LANGUAGE FlexibleInstances #-}
module Day22 where


import Utils (fromJust, getLines, splitOn, isJust)
import qualified Data.MultiSet as S


parseLine :: String -> Rule
parseLine s = (head ws == "on", ((head $ head cs, head cs!!1) , (head $ cs!!1, cs!!1!!1), (head $ cs!!2, cs!!2!!1)))
  where
    ws = words s
    xs = splitOn "," $ ws!!1
    cs :: [[Int]]
    cs = (read <$> ) . splitOn ".." . drop 2 <$> xs


type Line = (Int, Int)
type Rect = (Line, Line, Line)


intersectLine :: Line -> Line -> Maybe Line 
intersectLine (mina, maxa) (minb, maxb)
  | maxmin <= minmax = Just (maxmin, minmax)
  | otherwise = Nothing 
  where 
    maxmin = max mina minb 
    minmax = min maxa maxb


intersectRect :: Rect -> Rect -> Maybe Rect 
intersectRect (x1, y1, z1) (x2, y2, z2) = do 
  xline <- intersectLine x1 x2 
  yline <- intersectLine y1 y2 
  zline <- intersectLine z1 z2 
  return (xline, yline, zline)    


type Rule = (Bool, Rect)
data State = State { add :: S.MultiSet Rect , subtract :: S.MultiSet Rect }


-- add : |A ∪ B| = |A| + |B| - |A ∩ B| etc
applyRule :: State -> Rule -> State 
applyRule (State adds subtracts) (p, c) = 
  if p then 
    State (S.insert c $ S.union subIntersects adds) (S.union addIntersects subtracts)
  else
    State (S.union subIntersects adds) (S.union addIntersects subtracts) 
  where 
    addIntersects = S.map fromJust $ S.filter isJust (S.map (intersectRect c) adds)
    subIntersects = S.map fromJust $ S.filter isJust (S.map (intersectRect c) subtracts)


countLine :: Line -> Int
countLine (x,y) = y-x+1
countRect :: Rect -> Int 
countRect (x, y, z) = product $ map countLine [x, y, z]
countState :: State -> Int
countState (State adds subtracts) = sum (S.map countRect adds) - sum (S.map countRect subtracts)


day22 :: IO ()
day22 = do
  ls <- getLines 22
  let rules = parseLine <$> ls
      start = State S.empty S.empty
      solve = countState . foldl applyRule start

  putStrLn $ "Day22: part1: " ++ show (solve $ take 20 rules)
  putStrLn $ "Day22: part2: " ++ show (solve rules)
  return ()




