module Day23Old where


import Utils
import Algorithm.Search
import qualified Data.Map.Strict as M
import qualified Data.Bimap as B
import Data.Char
import Debug.Trace

input = [
    "#############"
  , "#12.4.6.8.01#"
  , "###B#A#A#D###"
  , "  #D#C#B#C#"
  , "  #########"]


type Node = Char
type Edges = Map Node [(Int, Node)]
type Pod = Char
type Cost = Int
type Positions = B.Bimap Pod Node

nodes :: [Node]
nodes = ['1','2','4','6','8','t','e','a','A','b','B','c','C','d','D']

isRoom :: Node -> Bool
isRoom n = n `elem` "aAbBcCdD"

edges :: Edges
edges = M.fromList es
  where
    es = [('1',[(1,'2')])
         ,('2',[(1,'1'),(2,'a'),(2,'4')])
         ,('4',[(2,'2'),(2,'6'),(2,'a'),(2,'b')]),('6',[(2,'4'),(2,'8'),(2,'b'),(2,'c')])
         ,('8',[(2,'6'),(2,'t'),(2,'c'),(2,'d')])
         ,('t',[(2,'8'),(1,'e'),(2,'d')])
         ,('e', [(1,'t')])
         ,('a',[(1,'A'),(2,'2'),(2,'4')]) ,('b',[(1,'B'),(2,'4'),(2,'6')])
         ,('c',[(1,'C'),(2,'6'),(2,'8')]),('d',[(1,'D'),(2,'8'),(2,'t')])
         , ('A',[(1,'a')]), ('B',[(1,'b')]), ('C',[(1,'c')]), ('D',[(1,'d')])
         ]


pods :: [Pod]
pods = ['A','B','C','D','a','b','c','d']

destinations :: Pod -> [Node]
destinations 'A' = "aA"
destinations 'B' = "bB"
destinations 'C' = "cC"
destinations 'D' = "dD"
destinations 'a' = "aA"
destinations 'b' = "bB"
destinations 'c' = "cC"
destinations 'd' = "dD"
destinations _ = error "No destination"

start ::Positions
start = B.fromList [('A', 'b'),('a','c'),('B','a'),('b','C'),('C','D'),('c','B'),('D','A'),('d','d')]

end ::Positions
end = B.fromList [('A', 'A'),('Z','a'),('B','B'),('Y','b'),('C','C'),('X','c'),('D','D'),('W','d')]

moveCost :: Pod -> Cost
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost 'a' = 1
moveCost 'b' = 10
moveCost 'c' = 100
moveCost 'd' = 1000
moveCost _ = error "No cost defined"

data State = State {
    spending :: Int
  , positions :: Positions
 } deriving (Eq, Show, Ord)


notOccupied :: State -> Node ->  Bool
notOccupied (State _ pm) n = n `B.notMemberR` pm


canMove :: Pod -> Node -> Bool
canMove 'A' p = p `elem` "aA12468te"
canMove 'B' p = p `elem` "bB12468te"
canMove 'C' p = p `elem` "cC12468te"
canMove 'D' p = p `elem` "dD12468te"
canMove 'a' p = p `elem` "aA12468te"
canMove 'b' p = p `elem` "bB12468te"
canMove 'c' p = p `elem` "cC12468te"
canMove 'd' p = p `elem` "dD12468te"
canMove _ _ = False


move :: Pod -> Node -> State -> [(Int, Node)]
move pod node st = first (* moveCost pod) <$> allowedMoves
  where
    moves = filter (notOccupied st . snd) $ edges M.! node
    allowedMoves
      | node `elem` "ABCD" = moves
      | node `elem` "abcd" = filter (\(_,n) -> n `elem` "ABCD" || not (isRoom n)) moves
      | otherwise = filter (\(_,n) -> n `elem` destinations pod) moves


moves :: State -> [State]
moves st@(State c pm) = do
  pod <- pods
  let node = pm B.! pod
  (cost, newNode) <- move pod node st
  return $ State (c+cost) (B.insert pod newNode pm)


cost :: State -> State -> Cost
cost (State c _) (State d _) = d - c

finished :: State -> Bool
finished s@(State c pm) = trace (show  pm) $ (==end) . positions $ s


day23 :: IO ()
day23 = do
  let startState = State 0 start
  putStrLn $ "Day23: part1: " ++ show (dijkstra (moves `pruning` (\s -> spending s >15300)) cost finished startState)
  putStrLn $ "Day23: part2: " ++ show ""

  return ()


{-

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
  print $ "Part 1 : " ++ show (fst $ fromMaybe (error "No solution") $ solve ["BD", "AC", "AB", "DC"])
  print $ "Part 2 : " ++ show (fst $ fromMaybe (error "No solution") $ solve ["BDDD", "ACBC", "ABAB", "DACC"])


type Room     = [Char]
type Rooms    = [Room]
type Hall     = [Char]
type State  = (Rooms, Hall)
type Amphibot = Char
type Cost     = Int

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

getRoom :: Rooms -> Int -> Room
getRoom rooms i
  | i > 3 = error "XXX"
  | otherwise = rooms !! i

canMoveHome :: Room -> Char -> Bool
canMoveHome r a = all (== a) r

canReachHome :: Hall -> Int -> Int -> Bool
canReachHome hall h r
  | r > 3 || h > 6 = error ("Jumped out of Room or Hall " ++ show r ++ "# " ++ show h)
  | r + 1 >= h + 1 = all (\i -> hall !! i == '.') [h+1 .. r+1]
  | h - 1 >= r + 2 = all (\i -> hall !! i == '.') [r+2 .. h-1]
  | otherwise      = True

moves :: State -> [State]
moves (rooms, hall) = intoHall ++ outOfHall
  where intoHall  = [(rs', h')
                    | i <- [0..3]
                    , let room = getRoom rooms i
                    , not (null room)                    -- Raum schon leer
                    , room /= replicate (length room) ("ABCD" !! i)  -- Raum schon richtig belegt
                    , let (a:as) = room
                    , j <- [0..6]
                    , reachable hall i j                 -- ist vom Raum der Punkt in der Halle erreichbar?
                    , let h' = replace hall j a
                    , let rs' = [if k == i then as else getRoom rooms k | k <- [0..3]]
                    ]
        outOfHall = [(rs', h')
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

{-| Kosten ermittelt anhand State **vor** Transition -}
costMove :: Int -> State -> Int -> Int -> Cost
costMove len (rooms,hall) roomNr hallNr = (pathCost !! roomNr !! hallNr + offset) * moveCost a
  where as = rooms !! roomNr
        a  = if hall !! hallNr /= '.' then hall !! hallNr else head as
        offset = if hall !! hallNr /= '.' then len - 1 - length as else len - length as

cost :: Int -> State -> State -> Cost
cost len (rs, h) (rs', h') = costMove len (rs, h) roomNr hallNr
  where roomNr = firstDifference rs rs'
        hallNr = firstDifference h h'

firstDifference :: Eq a => [a] -> [a] -> Int
firstDifference xs ys = head [i | i <- [0..length xs], xs !! i /= ys !! i]

finish :: Int -> State -> Bool
finish len zustand = fst zustand == map (replicate len) "ABCD"

solve :: Rooms -> Maybe (Cost, [State])
solve rooms = dijkstra moves (cost l) (finish l) (rooms, ".......")
  where l = length $ head rooms 

-}

