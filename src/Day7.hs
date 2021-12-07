
module Day7 where


import Utils (getLines, splitOn)
import Data.List (sort)


type Crab = Int


parseCrabs :: String -> [Crab]
parseCrabs s = read <$> splitOn "," s


median :: [a] -> a
median xs = if even len then xs!!l2 else xs!!(l2+1)
  where
    len = length xs
    l2 = len `div` 2


mean :: Foldable t => t Int -> Int
mean xs = sum xs `div` length xs


align :: (Int -> Int) -> [Crab] -> Int
align metric cs = go 500
  where
    go :: Int -> Int
    go n
      | fuel < upFuel && fuel < dnFuel = fuel
      | upFuel > dnFuel = go (n-1)
      | otherwise = go (n+1)
      where
        fuel = fuelCalc metric n cs
        upFuel = fuelCalc metric (n+1) cs
        dnFuel = fuelCalc metric (n-1) cs


fuelCalc :: (Int -> Int) -> Int -> [Crab] -> Int
fuelCalc metric n cs = sum $ (\c -> metric $ abs (c-n)) <$> cs 


day7 :: IO ()
day7 = do
  inLines <- getLines 7
  let cs :: [Crab]
      cs = sort $ parseCrabs $ head inLines
  putStrLn $ "Day7: part1: " ++ show (fuelCalc id (median cs) cs)
  putStrLn $ "Day7: part2: " ++ show (fuelCalc (\x -> x*(x+1)`div`2) (mean cs) cs)
  putStrLn $ "Day7: part1: " ++ show (align id cs) 
  putStrLn $ "Day7: part2: " ++ show (align (\x -> x*(x+1)`div`2) cs) 

  return ()


