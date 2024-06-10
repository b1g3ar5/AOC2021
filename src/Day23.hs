{-# LANGUAGE LambdaCase, FlexibleInstances, NamedFieldPuns #-}


module Day23 where

import Utils (Set, fromJust)
import HeapIndexed (Heap, singleton, union, fromList, extractMin)
import qualified Data.Set as S
import qualified Data.IntMap as IM


type Room = [Pod] -- Who's in the room
type Rooms = [Room]
type Hall = [Pod] -- Who's in the hall
type State = (Rooms, Hall, Cost, Int)
type Pod = Char
type Cost = Int


instance Semigroup Cost where
  c <> d = c+d


instance Monoid Cost where
  mempty = 0


-- From the top of the room
pathDistance :: [[Int]]
pathDistance = [ [3,2,2,4,6,8,9] -- A
           , [5,4,2,2,4,6,7] -- B
           , [7,6,4,2,2,4,5] -- C
           , [9,8,6,4,2,2,3] -- D
           ]


roomFor :: Pod -> Int
roomFor 'A' = 0
roomFor 'B' = 1
roomFor 'C' = 2
roomFor 'D' = 3
roomFor _ = error "Unknown room"


-- Check that the path to the hall is empty
reachable :: Hall -> Int -> Int -> Bool
reachable hall r h
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h     = all (\i -> hall !! i == '.') [h .. r+1]
  | otherwise      = all (\i -> hall !! i == '.') [r+2 .. h]


-- replace and item in a list
replace :: [a] -> Int -> a -> [a]
replace xs i c = [if j == i then c else x | (j, x) <- zip [0..] xs]


-- Check no other pods in the room
canMoveHome :: Room -> Pod -> Bool
canMoveHome r p = all (== p) r


-- Check that the path home is empty
canReachHome :: Hall -> Int -> Int -> Bool
canReachHome hall h r
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h + 1 = all (\i -> hall !! i == '.') [h+1 .. r+1]
  | h - 1 >= r + 2 = all (\i -> hall !! i == '.') [r+2 .. h-1]
  | otherwise      = True


moves :: State -> [State]
moves (rooms, hall, cost, roomLength) = intoHall ++ outOfHall
  where intoHall  = [(newRooms, newHall, 0, roomLength)
                    | i <- [0..3] -- room position
                    , let room = rooms !! i
                    , not (null room)
                    , room /= replicate (length room) ("ABCD" !! i)
                    , let (pod:pods) = room
                    , j <- [0..6] -- hall position
                    , reachable hall i j 
                    , let newHall = replace hall j pod
                    , let newRooms = [if k == i then pods else rooms !! k | k <- [0..3]]
                    ]
        outOfHall = [(rs', h',0, roomLength)
                    | i <- [0..6] -- hall position
                    , let pod = hall !! i
                    , pod /= '.'
                    , let h' = replace hall i '.'
                    , let j = roomFor pod
                    , let r'' = rooms !! j
                    , canMoveHome r'' pod
                    , canReachHome hall i j
                    , let r' = pod : r''
                    , let rs' = replace rooms j r'
                    ]


moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost _ = error "Unknown amphibot in moveCost"


costMove :: State -> Int -> Int -> Int
costMove (rooms, hall, cost, roomLength) roomNr hallNr = (pathDistance !! roomNr !! hallNr + offset) * moveCost movingPod
  where podsInRoom = rooms !! roomNr
                                                  -- hall to room      room to hall
        movingPod  = if hall !! hallNr /= '.' then hall !! hallNr else head podsInRoom
        offset = if hall !! hallNr /= '.' then roomLength - 1 - length podsInRoom else roomLength - length podsInRoom


cost :: State -> State -> Int
cost (rs, h, c, l) (rs', h', c', l') = costMove (rs, h, c, l) roomNr hallNr
  where roomNr = firstDifference rs rs'
        hallNr = firstDifference h h'


firstDifference :: Eq a => [a] -> [a] -> Int
firstDifference xs ys = head [i | i <- [0..length xs], xs !! i /= ys !! i]


finish :: State -> Bool
finish state@(r,h,c,l) = r == map (replicate l) "ABCD"


solve :: Rooms -> Cost
solve rooms = dijkstra (singleton 0 startState) S.empty finish
  where 
    startState :: State
    startState = (rooms, ".......", 0, length $ rooms !!0)


dijkstra :: Heap Int State -> Set State -> (State -> Bool) -> Cost
dijkstra pipeline visited finished
  | null pipeline = mempty
  | finished state = savedMin
  | state `S.member` visited = dijkstra remainingPipeline visited finished
  | otherwise = dijkstra newPipeline (S.insert state visited) finished
  where
    ((savedMin, state), remainingPipeline) = fromJust $ extractMin pipeline
    newStates = filter (`S.notMember` visited) $ moves state
    newPipeline = remainingPipeline `union` fromList ((\n -> (savedMin <> cost state n, n)) <$> newStates)


day23 :: IO ()
day23 = do
  putStrLn $ "Day23: Part 1 : " ++ show (solve ["BD", "AC", "AB", "DC"])
  putStrLn $ "Day23: Part 2 : " ++ show (solve ["BDDD", "ACBC", "ABAB", "DACC"])


