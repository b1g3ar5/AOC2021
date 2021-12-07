
module Day7 where


import Utils (getLines, splitOn)


type Crab = Int


parseCrabs :: String -> [Crab]
parseCrabs s = read <$> splitOn "," s


align1 :: [Crab] -> Int
align1 cs = go 500
  where
    go :: Int -> Int
    go n
      | bigger > (smaller + equal) = go (n+1)
      | smaller > (bigger + equal) = go (n-1)
      | otherwise = sum $ (\c -> abs (c-n)) <$> cs
      where
        bigger = length $ filter (>n) cs
        smaller = length $ filter (<n) cs
        equal = length $ filter (==n) cs


align2 :: [Crab] -> Int
align2 cs = go 500
  where
    go :: Int -> Int
    go n
      | fuel < upFuel && fuel < dnFuel = fuel
      | upFuel > dnFuel = go (n-1)
      | otherwise = go (n+1)
      where
        fuel = fuel2 n cs
        upFuel = fuel2 (n+1) cs
        dnFuel = fuel2 (n-1) cs


fuel2 :: Int -> [Crab] -> Int
fuel2 n cs = sum $ go <$> cs
  where
    go c = d * (d+1) `div` 2
      where
        d = abs (c-n)


day7 :: IO ()
day7 = do
  inLines <- getLines 7
  let cs :: [Crab]
      cs = parseCrabs $ head inLines
  putStrLn $ "Day7: part1: " ++ show (align1 cs)
  putStrLn $ "Day7: part2: " ++ show (align2 cs) 

  return ()


