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



