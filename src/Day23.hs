{-# LANGUAGE LambdaCase, NamedFieldPuns #-}


module Day23 where


import Utils hiding (replace)
import Algorithm.Search ( dijkstra )
--import qualified Data.Map.Strict as M
--import qualified Data.Bimap as B
import qualified Data.IntMap as IM
--import qualified Data.Vector as V

import Debug.Trace

day23 :: IO ()
day23 = do
  putStrLn $ "Part 1 : " ++ show (fst $ fromMaybe (error "No solution") $ solve ["BD", "AC", "AB", "DC"])
  putStrLn $ "Part 2 : " ++ show (fst $ fromMaybe (error "No solution") $ solve ["BDDD", "ACBC", "ABAB", "DACC"])


type Room = [Char]
type Rooms = [Room]
type Hall = [Char]
type State = (Rooms, Hall, Int)
type Pod = Char
type Cost = Int

-- From the top of the room
pathCost :: [[Cost]]
pathCost = [ [3,2,2,4,6,8,9] -- A
           , [5,4,2,2,4,6,7] -- B
           , [7,6,4,2,2,4,5] -- C
           , [9,8,6,4,2,2,3] -- D
           ]

roomFor :: Char -> Int
roomFor 'A' = 0
roomFor 'B' = 1
roomFor 'C' = 2
roomFor 'D' = 3
roomFor _ = error "Unknown room"

reachable :: Hall -> Int -> Int -> Bool
reachable hall r h
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h     = all (\i -> hall !! i == '.') [h .. r+1]
  | otherwise      = all (\i -> hall !! i == '.') [r+2 .. h]


replace :: [a] -> Int -> a -> [a]
replace xs i c = [if j == i then c else x | (j, x) <- zip [0..] xs]


canMoveHome :: Room -> Char -> Bool
canMoveHome r a = all (== a) r


canReachHome :: Hall -> Int -> Int -> Bool
canReachHome hall h r
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h + 1 = all (\i -> hall !! i == '.') [h+1 .. r+1]
  | h - 1 >= r + 2 = all (\i -> hall !! i == '.') [r+2 .. h-1]
  | otherwise      = True

moves :: State -> [State]
moves (rooms, hall, cost) = intoHall ++ outOfHall
  where intoHall  = [(newRooms, newHall, 0)
                    | i <- [0..3]
                    , let room = rooms !! i
                    , not (null room)
                    , room /= replicate (length room) ("ABCD" !! i)
                    , let (a:as) = room
                    , j <- [0..6]
                    , reachable hall i j 
                    , let newHall = replace hall j a
                    , let newRooms = [if k == i then as else rooms !! k | k <- [0..3]]
                    ]
        outOfHall = [(rs', h',0)
                    | i <- [0..6]
                    , let a = hall !! i
                    , a /= '.'
                    , let h' = replace hall i '.'
                    , let j = roomFor a
                    , let r'' = rooms !! j
                    , canMoveHome r'' a
                    , canReachHome hall i j
                    , let r' = a : r''
                    , let rs' = replace rooms j r'
                    ]

moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost _ = error "Unknown amphibot in moveCost"

costMove :: Int -> State -> Int -> Int -> Cost
costMove len (rooms, hall, cost) roomNr hallNr = (pathCost !! roomNr !! hallNr + offset) * moveCost movingPod
  where podsInRoom = rooms !! roomNr
                                                  -- hall to room      room to hall
        movingPod  = if hall !! hallNr /= '.' then hall !! hallNr else head podsInRoom
        offset = if hall !! hallNr /= '.' then len - 1 - length podsInRoom else len - length podsInRoom

cost :: Int -> State -> State -> Cost
cost len (rs, h, c) (rs', h', c') = costMove len (rs, h, c) roomNr hallNr
  where roomNr = firstDifference rs rs'
        hallNr = firstDifference h h'

firstDifference :: Eq a => [a] -> [a] -> Int
firstDifference xs ys = head [i | i <- [0..length xs], xs !! i /= ys !! i]

finish :: Int -> State -> Bool
finish len state@(r,h,c) = r == map (replicate len) "ABCD"

solve :: Rooms -> Maybe (Cost, [State])
solve rooms = dijkstra moves (cost l) (finish l) (rooms, ".......", 0)
  where l = length $ head rooms 