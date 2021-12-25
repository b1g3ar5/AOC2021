{-# LANGUAGE LambdaCase, NamedFieldPuns #-}


module Day23 where


import Utils hiding (replace)
import Algorithm.Search
import qualified Data.Map.Strict as M
import qualified Data.Bimap as B

import Debug.Trace

-- start = ["BA", "CD", "BC", "DA"]

day23 :: IO ()
day23 = do
  print $ "Part 1 : " ++ show (solve ["BD", "AC", "AB", "DC"])
  print $ "Part 2 : " ++ show (solve ["BDDD", "ACBC", "ABAB", "DACC"])

{-
#01#2#3#4#56#
#..o.o.o.o..#
###A#C#B#C###
   0 1 2 3
-}

type Room     = [Char]
type Rooms    = [Room]
type Hall     = B.Bimap Int Char
data State  = State {spending :: Int, rooms::Rooms, hall :: Hall} deriving (Ord, Eq, Show)
type Pod = Char
type Cost     = Int

pathCost :: [[Cost]]
pathCost = [
          [3,2,2,4,6,8,9],
          [5,4,2,2,4,6,7],
          [7,6,4,2,2,4,5],
          [9,8,6,4,2,2,3]
       ]

roomFor :: Char -> Int
roomFor 'A' = 0
roomFor 'B' = 1
roomFor 'C' = 2
roomFor 'D' = 3
roomFor _ = error "Not a room"

reachable :: Hall -> Int -> Int -> Bool
reachable hall rix hix
  | rix > 3 || hix > 6 = error ("Index error " ++ show rix ++ "# " ++ show hix)
  | rix + 1 >= hix     = all (`B.notMember` hall) [hix .. rix+1]
  | otherwise      = all (`B.notMember` hall) [rix+2 .. hix]


replace :: [a] -> Int -> a -> [a]
replace xs i c = [if j == i then c else x | (j, x) <- zip [0..] xs]


canMoveHome :: Room -> Char -> Bool
canMoveHome r a = all (== a) r

canReachHome :: Hall -> Int -> Int -> Bool
canReachHome hall h r
  | r > 3 || h > 6 = error ("Index error " ++ show r ++ "# " ++ show h)
  | r + 1 >= h + 1 = all (`B.notMember` hall) [h+1 .. r+1]
  | h - 1 >= r + 2 = all (`B.notMember` hall) [r+2 .. h-1]
  | otherwise      = True

moves :: State -> [State]
moves st@(State c rooms hall) = intoHall ++ outOfHall
  where intoHall  = [State (c+newSpend) rs' h'
                    | rix <- [0..3]
                    , let room = rooms !! rix
                    , not (null room)
                    , room /= replicate (length room) ("ABCD" !! rix)
                    , let (a:as) = room
                    , hix <- [0..6]
                    , reachable hall rix hix
                    , let h' = B.insert hix a hall --replace hall j a
                    , let rs' = [if k == rix then as else rooms !! k | k <- [0..3]]
                    , let newSpend = costMove 2 st rix hix
                    ]
        outOfHall = [State 0 newRooms newHall
                    | hix <- B.keys hall 
                    , let pod = hall B.! hix
                    , let newHall = B.delete hix hall
                    , let rix = roomFor pod
                    , let r'' = rooms !! rix
                    , canMoveHome r'' pod
                    , canReachHome hall hix rix
                    , let r' = pod : r''
                    , let newRooms = replace rooms rix r'
                    , let newSpend = costMove 2 st rix hix

                    ]

moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost _ = error "Not a pod"


costMove :: Int -> State -> Int -> Int -> Cost
costMove len (State _ rooms hall) roomNr hallNr = (pathCost !! roomNr !! hallNr + offset) * moveCost a
  where as = rooms !! roomNr
        a  = if hallNr `B.member` hall then hall B.! hallNr else head as
        offset = if hallNr `B.member` hall then len - 1 - length as else len - length as


{-
costTransition :: Int -> State -> State -> Cost
costTransition len (State _ rs h) (State _ rs' h') = costMove len (State 0 rs h) roomNr hallNr
  where roomNr = firstDifference rs rs'
        hallNr = firstDifference' h h'


firstDifference :: Eq a => [a] -> [a] -> Int
firstDifference xs ys = head [i | i <- [0..length xs], xs !! i /= ys !! i]

firstDifference' :: (Ord a, Eq a) => B.Bimap Int a -> B.Bimap Int a -> Int
firstDifference' xs ys = head [xi | xi <- B.keys xs, yi <- B.keys ys, xs B.! xi /= ys B.! yi]
-}


cost :: Int -> State -> State -> Cost
cost len (State c rs h) (State d rs' h') = d - c


solutionFound :: Int -> State -> Bool
solutionFound len (State _ rooms hall) = rooms == map (replicate len) "ABCD"

solve :: Rooms -> Maybe (Cost, [State])
solve rooms = dijkstra moves (cost l) (solutionFound l) (State 0 rooms B.empty) --".......")
  where l = length $ head rooms 


{-

input = [
    "#############"
  , "#01.3.5.7.90#"
  , "###B#A#A#D###"
  , "  #D#C#B#C#"
  , "  #########"]


type Room = String
type Pod = Char

toRoom :: Pod -> RoomIx
toRoom 'A' = 0
toRoom 'B' = 1
toRoom 'C' = 2
toRoom 'D' = 3
toRoom c = error $ "Not a pod: " ++ [c]


roomSize = 2

data State = State {
  spending :: Int
  , rooms :: [Room]
  , hall :: B.Bimap Int Pod
} deriving (Show, Eq, Ord)

hallKeys :: [Int]
hallKeys = [0,1,3,5,7,9,10]

start :: [Room]
--start = ["BD", "AC", "AB", "DC"]
start = ["BA", "CD", "BC", "A"]
end :: [Room]
end = ["AA", "BB", "CC", "DD"]


moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost _ = error "No cost defined"


type Dist = Int
type Pos = Int
type RoomIx = Int
type HallIx = Int


distance:: RoomIx -> HallIx -> Dist
distance rix hix = abs $ (rix+1)*2-hix

path :: RoomIx -> HallIx -> [HallIx]
path rix hix = [hix, hix + signum (x - hix)..x]
  where
    x = (rix+1)*2


leaveRoom :: State -> RoomIx -> HallIx -> Maybe State
leaveRoom (State c rooms hall) rix hix
  | null (rooms !! rix) = Nothing
  | otherwise = Just $ State (c+cost) newRooms newHall
  where
    room@(pod:others) = rooms !! rix
    newRooms = (\(ix, rm) -> if ix==rix then others else rm) <$> zip [0..] rooms
    newHall = B.insert hix pod hall
    cost = (distance rix hix + roomSize - length room) * moveCost pod


enterRoom :: State -> HallIx -> State
enterRoom (State c rooms hall) hix = State (c+cost) newRooms newHall
  where
    pod = hall B.! hix
    rix = toRoom pod
    newRooms = (\(ix, rm) -> if ix==rix then pod:rm else rm) <$> zip [0..] rooms
    newHall = B.deleteR pod hall
    cost = (distance rix hix + roomSize - length (rooms !! rix)) * moveCost pod



moves :: State -> [State]
moves st@(State c rooms hall) = entries ++ catMaybes exits
  where
    entries = do
      hix <- B.keys hall 
      let pod = hall B.! hix
          rix = toRoom pod
          p = filter (/=hix) $ path rix hix
      guard $ length (rooms !! rix) < roomSize -- Room not full
      guard $ and $ (`B.notMember` hall) <$> p -- Path isempty
      guard $ and $ (==pod) <$> rooms !! rix -- no foreigners
      return $ enterRoom st hix
    exits = do
      rix <- [0..3]
      hix <- hallKeys
      let rm = rooms !! rix
          p = path rix hix
      guard $ and $ (`B.notMember` hall) <$> p -- Path isempty
      return $ fromMaybe Nothing $ Just $ leaveRoom st rix hix


cost :: State -> State -> Int
cost (State c _ _) (State d _ _) = d - c

finished :: State -> Bool
finished s@(State spending rooms hall) = trace (show $ rooms) $ rooms==end


day23 :: IO ()
day23 = do
  let startState = State 0 start (B.insert 9 'D' B.empty)
  putStrLn $ "Day23: part1: " ++ show (dijkstra (moves `pruning` (\s -> spending s >15300)) cost finished startState)
  --putStrLn $ "Day23: part2: " ++ show (hall <$> moves (moves startState!!1))
  --putStrLn $ "Day23: part2: " ++ show (hall <$> moves (moves startState!!2))
  --putStrLn $ "Day23: part2: " ++ show (hall <$> moves (moves startState!!3))
  --putStrLn $ "Day23: part2: " ++ show (hall <$> moves (moves startState!!4))
  --putStrLn $ "Day23: part2: " ++ show (hall <$> moves (moves startState!!5))
  --putStrLn $ "Day23: part2: " ++ show (hall <$> moves (moves startState!!6))

  return ()

-}