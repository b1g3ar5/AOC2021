module Day13 where


import Utils (getLines, nub, Coord, splitOn)


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


parse :: [String] -> ([Coord], [Fold])
parse = go ([],[])
  where
    go :: ([Coord], [Fold]) -> [String] -> ([Coord], [Fold])
    go acc [] = acc
    go (cs, []) ([]:ls) = (cs, parseFold <$> ls)
    go (cs, fs) (l:ls) = go (parseCoord l:cs, fs) ls
    

makeFold :: [Coord] -> Fold -> [Coord]
makeFold cs (X, n) = nub $ (\(x,y) -> if x>n then (2*n-x, y) else (x,y)) <$> cs
makeFold cs (Y, n) = nub $ (\(x,y) -> if y>n then (x, 2*n-y) else (x,y)) <$> cs

render :: [Coord] -> String
render cs = unlines [[if (x,y) `elem` cs then '#' else ' ' | x <- [0..mx]] | y <- [0..my]]
  where
    mx = maximum $ fst <$> cs
    my = maximum $ snd <$> cs

day13 :: IO ()
day13 = do
  ls <- getLines 13
  let (cs, fs) = parse ls
  putStrLn $ "Day13: part1: " ++ show (length $ makeFold cs $ head fs)
  putStrLn $ "Day13: part2:\n" ++ render (foldl makeFold cs fs) 
  -- JZGUAPRB

  return ()


test = ["6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along y=7"
  , "fold along x=5"]


{-

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
-}