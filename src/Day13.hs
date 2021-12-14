module Day13 where


import Prelude hiding (map)
import Utils (getLines, nub, Coord, splitOn)
import Data.Set (Set, empty, insert, map)


data Axis = X | Y deriving (Eq, Show)
type Fold = (Axis, Int)


parseCoord :: String -> Coord
parseCoord s = (read $ head ws, read $ ws!!1)
  where
    ws = splitOn "," s


parseFold :: String -> (Axis, Int)
parseFold s = (if head ps == "x" then X else Y, read $ ps!!1)
  where
    ws = words s
    ps = splitOn "=" $ ws!!2


parse :: [String] -> (Set Coord, [Fold])
parse = go (empty,[])
  where
    go :: (Set Coord, [Fold]) -> [String] -> (Set Coord, [Fold])
    go acc [] = acc
    go (cs, []) ([]:ls) = (cs, parseFold <$> ls) -- empty line
    go (cs, fs) (l:ls) = go (insert (parseCoord l) cs, fs) ls
    

makeFold :: Set Coord -> Fold -> Set Coord
makeFold cs (X, n) = map (\(x,y) -> if x>n then (2*n-x, y) else (x,y)) cs
makeFold cs (Y, n) = map (\(x,y) -> if y>n then (x, 2*n-y) else (x,y)) cs


render :: Set Coord -> String
render cs = unlines [[if (x,y) `elem` cs then '#' else ' ' | x <- [0..mx]] | y <- [0..my]]
  where
    (mx, my) = (maximum $ map fst cs, maximum $ map snd cs)


day13 :: IO ()
day13 = do
  ls <- getLines 13
  let (cs, fs) = parse ls
  putStrLn $ "Day13: part1: " ++ show (length $ makeFold cs $ head fs)
  putStrLn $ "Day13: part2:\n" ++ render (foldl makeFold cs fs) -- JZGUAPRB

  return ()

