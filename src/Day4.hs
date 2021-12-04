
module Day4 where

import Data.Map.Strict (Map)
import qualified Data.Map as M
import Utils (getLines, Coord, chunksOf, first)


type Board a = Map Coord a


parseBoard :: [String] -> Board Int
parseBoard = go 0 M.empty
  where
    go :: Int -> Board Int -> [String] -> Board Int
    go n b [] = b
    go n b ls = go (n+1) (M.union b (M.fromList $ zip ((n,) <$> [0..]) ns)) $ tail ls
      where
        ns :: [Int]
        ns = read <$> words (head ls)


isFinished :: Board (Bool, a) -> Bool
isFinished b = or $ [rowIsFinished, colIsFinished] <*> [0..4]
  where
    rowIsFinished :: Int -> Bool
    rowIsFinished n = and $ fst . (b M.!) . (,n) <$> [0..4]
    colIsFinished :: Int -> Bool
    colIsFinished n = and $ fst . (b M.!) . (n,) <$> [0..4]


mark :: Eq a => a -> Board (Bool, a) -> Board (Bool,a)
mark x = M.map (\(p,y) -> ((y==x) || p,y))


play1 :: [Int] -> [Board (Bool, Int)] -> Int
play1 [] _ = error "We ran out of numbers"
play1 (n:ns) bs
  | not . null $ finished = score n $ head finished
  | otherwise = play1 ns newBs
  where
    newBs = mark n <$> bs
    finished = filter isFinished newBs


play2 :: [Int] -> [Board (Bool, Int)] -> Int
play2 [] _ = error "We ran out of numbers"
play2 _ [] = error "We ran out of boards"
play2 (n:ns) bs
  | null newBs = score n $ mark n $ head bs
  | otherwise = play2 ns newBs
  where
    newBs = filter (not . isFinished) $ mark n <$> bs


score :: Int -> Board (Bool, Int) -> Int
score x b = x * foldr (\(p,y) acc -> if p then acc else acc + y) 0 b


day4 :: IO ()
day4 = do
  inLines <- getLines 4
  let numbers :: [Int] 
      numbers = (read <$>) . words $ (\c -> if c==',' then ' ' else c) <$> head inLines
      boards :: [Board (Bool, Int)]
      boards = (M.map (False,) <$> ) . (parseBoard <$>) . (tail <$>) <$> chunksOf 6 $ tail inLines
  putStrLn $ "Day4: part1: " ++ show (play1 numbers boards)
  putStrLn $ "Day4: part2: " ++ show (play2 numbers boards)
  return ()

