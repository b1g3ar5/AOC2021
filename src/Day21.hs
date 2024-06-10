module Day21 where


import Memo (memoFix)
import Utils (swap)


type State = ((Int, Int), (Int,Int))


deterministicPlay :: (Int, State) -> Int
deterministicPlay (rolls, state@((score1, score2), (pos1, pos2)))
  | score1 >= 1000 = rolls * score2
  | score2 >= 1000 = rolls * score1
  | otherwise = deterministicPlay (rolls+3, ((score2, score1 + newPos), (pos2, newPos)))
  where
    move = rolls*3 + 6
    newPos = 1 + (pos1 + move - 1) `mod` 10 


tupleSum :: Num a => [(a,a)] -> (a,a)
tupleSum = foldl1 (\(x,y) (l,m) -> (x+l, y+m))


-- Factored out the recursion so that I can use memoisation
diracPlayF :: (State -> (Int, Int)) -> State -> (Int, Int)
diracPlayF f state@((s1, s2), (p1, p2))
  | s1>=21 = (1,0)
  | s2>=21 = (0,1)
  | otherwise = swap $ tupleSum ( (\r -> f ((s2, s1+newP p1 r),(p2, newP p1 r))) <$> diracDie)
  where
    newP p r = 1 + (p + r - 1) `mod` 10 


diracDie :: [Int]
diracDie = [r1+r2+r3 | r1 <-[1,2,3], r2 <-[1,2,3], r3 <-[1,2,3]]


day21 :: IO ()
day21 = do
  let start ::State
      start = ((0,0),(1,2))
  
  putStrLn $ "Day21: part1: " ++ show (deterministicPlay (0, start))
  putStrLn $ "Day21: part2: " ++ show (memoFix diracPlayF start)
  

  return ()

