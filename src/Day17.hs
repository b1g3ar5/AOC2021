module Day17 where


import Utils (guard, Coord)


minx, maxx, maxy, miny :: Int
minx = 236
maxx = 262
miny = -78
maxy = -58


inTarget, passedTarget:: State -> Bool
inTarget ((x, y),_,_) = x>=minx && x<=maxx && y>=miny && y<=maxy
passedTarget ((x, y),_,_) = y<miny || x>maxx


type State = ((Int, Int), (Int, Int), Int)


update :: State -> State
update ((x,y), (v,w), maxy) = ((x+v, y+w),(v-signum v, w-1), max maxy (y+w)) 


run :: State -> (Bool, Int)
run s@(_,_,my)
  | inTarget s = (True, my)
  | passedTarget s = (False, my)
  | otherwise = run $ update s


findHighest:: Coord -> Int
findHighest s@(_,y) = maximum ps
  where
    maxtry = 100
    ps :: [Int]
    ps = do
      v<-[0..maxtry] -- set off in positive x direction
      w<-[(-10)..maxtry]
      let (p,m) = run (s, (v,w), y)
      guard p
      return m


findCount :: Coord -> Int
findCount s@(_,y) = length ps
  where
    ps :: [Coord]
    ps = do
      v<-[0..maxx]
      w<-[miny..300]
      let (p,m) = run (s, (v,w), y)
      guard p
      return (v,w)


day17 :: IO ()
day17 = do
  putStrLn $ "Day17: part1: " ++ show (findHighest (0,0))
  putStrLn $ "Day17: part1: " ++ show (findCount (0,0))

  return ()

