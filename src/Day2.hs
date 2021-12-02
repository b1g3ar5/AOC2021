
module Day2 where


import Utils


data Move = Fw Int | Dn Int | Up Int deriving (Show, Eq)


type Pos = (Int, Int)
type Aim = Int
type State = (Pos, Aim)


move1 :: Pos -> Move -> Pos
move1 (x,y) (Fw z) = (x+z,y)
move1 (x,y) (Dn z) = (x,y+z)
move1 (x,y) (Up z) = (x,y-z)


move2 :: State -> Move -> State
move2 ((x,y), a) (Fw z) = ((x+z,y+a*z), a)
move2 ((x,y), a) (Dn z) = ((x,y), a+z)
move2 ((x,y), a) (Up z) = ((x,y), a-z)


parseMove :: String -> Move
parseMove s
  | head ws == "forward" = Fw $ read $ ws!!1
  | head ws == "up" = Up $ read $ ws!!1
  | head ws == "down" = Dn $ read $ ws!!1
  | otherwise = error $ "parseMove only parses forward, up and down, not: " ++ show s
  where
    ws = words s


day2 :: IO ()
day2 = do
  inLines <- getLines 2
  let moves = parseMove <$> inLines
  putStrLn $ "Day2: part1: " ++ show (uncurry (*) $ foldl move1 (0,0) moves)
  putStrLn $ "Day2: part2: " ++ show (uncurry (*) $ fst $ foldl move2 ((0,0),0) moves)

  return ()


