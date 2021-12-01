
module Day1 where


import Utils


day1 :: IO ()
day1 = do
  inLines <- getLines 1
  let ns :: [Int]
      ns = read <$> inLines
      increases :: [Int] -> Int
      increases xs = length $ filter id $ zipWith (>) (tail xs) (init xs)
      sum3 = zipWith3 (\x y z -> x+y+z) (init $ init ns) (init $ tail ns) (tail $ tail ns)

  putStrLn $ "Day1: part1: " ++ show (increases ns)
  putStrLn $ "Day1: part2: " ++ show (increases sum3)

  return ()


