module Day25 where


import Utils hiding (Empty)
import qualified Data.Map.Strict as M

-- Simple but very slow...

isCuc, notCuc :: Char -> Bool
isCuc c = c `elem` ">v"
notCuc = not . isCuc

move :: Char -> String -> String
move cuc cs = go (head cs, []) cs
  where
    go :: (Char, String) -> String -> String
    go (h, acc) [] = acc
    go (h, c:cs) [l]
      | notCuc h && l==cuc = l:cs ++ [h]
      | otherwise = c:cs ++ [l]
    go (h, acc) (x : y : ys)
      | x==cuc && isCuc y = go (h, acc ++ [cuc]) (y:ys)
      | x==cuc = go (h, acc ++ ['.',cuc]) ys
    go (h, acc) (y:ys) = go (h, acc ++ [y]) ys


moveSouth, moveEast :: String ->String
moveSouth = move 'v'
moveEast = move '>'


loop :: [String] -> [String]
loop ls = transpose $ moveSouth <$> transpose (moveEast <$> ls)


steadyStateWithCount :: Eq a => Int -> (a -> a) -> a -> (Int, a)
steadyStateWithCount n f x = if f x == x then (n+1,x) else steadyStateWithCount (n+1) f (f x)


day25 :: IO ()
day25 = do
  ls <- getLines 25
  putStrLn $ "Day25: part1:\n" ++ show (fst $ steadyStateWithCount 0 loop ls)

  return ()
