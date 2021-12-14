module Day12 where


import Utils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Alg (coalg)
import Text.Read (Lexeme(String))


type Node = String
type Edge = (Node, Node)
type Graph = Map Node [Node]


mkGraph :: [Edge] -> Graph
mkGraph es = go M.empty es
  where
    go :: Graph -> [Edge] -> Graph
    go mp [] = mp
    go mp (e:es) = go (insert2 mp e) es
    -- We could leave out the edges back to start and back from end
    -- But the path logic mewans that they are not used anyway...
    insert2 :: Graph -> Edge -> Graph
    insert2 mp (to, from) = M.insertWith (++) from [to] $ M.insertWith (++) to [from] mp


parseEdge :: String -> Edge
parseEdge s = (head ps, ps!!1)
  where
    ps = splitOn "-" s


type Path = [Node]


isSmall :: Node -> Bool
isSmall = isAsciiLower . head


-- The first parameter is whether we allow a second visit to a small cave
generatePaths :: Bool -> Node -> Node -> Graph -> [[Node]]
generatePaths isPart1 start end g = map reverse $ dfs isPart1 [] [[]] start
  where
    -- Parameters: 
    -- whether second visits are allowed
    -- list of nodes visited
    -- paths that we have accumulated
    -- the next node
    dfs :: Bool -> [Node] -> [[Node]] -> Node -> [[Node]]
    dfs secondVisitNotAllowed visited acc next
      | next == end = map (next :) acc -- add end to all the paths
      | next == start && not (null visited) = []  -- don't go back to the start
      | secondVisitNotAllowed && isSecondVisit = [] -- second visit is not allowed or has already occurred
        -- add next to all the paths, recurse into all the kids, concat the results
      | otherwise = concatMap (dfs (secondVisitNotAllowed || isSecondVisit) (next:visited) (map (next :) acc)) $ g M.! next
      where
        isSecondVisit = isSmall next && next `elem` visited


day12 :: IO ()
day12 = do
  ls <- getLines 12
  let ls = test'
  let graph = mkGraph $ parseEdge <$> ls
  putStrLn $ "Day12: part1: " ++ show (length $ generatePaths True "start" "end" graph )
  putStrLn $ "Day12: part2: " ++ unlines (sort $ show <$> generatePaths False "start" "end" graph)

  -- Hylomorphism version
  putStrLn $ "Tree: " ++ show (length $ filter (\p -> last p == "end") $ hylo pathAlg makeCoalg ("start", [], graph))
  putStrLn $ "Tree: " ++ show (length $ filter (\p -> last p == "end") $ hylo pathAlg makeCoalg' ("start", [], graph, True))
  --putStrLn $ "Tree:\n" ++ unlines (sort $ filter (\p -> last p == 'd') $ hylo renderAlg makeCoalg' ("start", [], graph, True))

  return ()


test = ["start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"]

test' = ["dc-end"
  , "HN-start"
  , "start-kj"
  , "dc-start"
  , "dc-HN"
  , "LN-dc"
  , "HN-end"
  , "kj-sa"
  , "kj-HN"
  , "kj-dc"]

newtype Fix f = Fix { unFix :: f (Fix f) } 
type Algebra f a = f a -> a
type Coalgebra f a = (a -> f a)
type CVCoalgebra f a = a -> f (Either (Fix f) a)


cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

apo :: Functor f => CVCoalgebra f a -> a -> Fix f
apo f = Fix . fmap (either id (apo f)) . f


--fix :: (a -> a) -> a
--fix f = let {x = f x} in x


type Seed = (Node, [Node], Graph)
type Seed' = (Node, [Node], Graph, Bool)
data TreeF a r = TNode a [r] deriving (Functor, Eq, Show)
type Tree a = Fix (TreeF a)


mkTree :: Graph -> Tree Node
mkTree g = ana makeCoalg ("start", [], g)


-- I couldn't work out how to exclude dead end caves that aren't "end"
makeCoalg :: Seed -> TreeF Node Seed
makeCoalg (this, visited, g)
  | this == "end" = TNode this []
  | otherwise = TNode this $ (, this:visited, g) <$> kids
  where
    visitAllowed x = not (isSmall x) || x `notElem` visited
    kids = filter visitAllowed $ g M.! this


-- This won't work because we can't let just one of the kids to make a second visit
-- So, I guess we cant use ana we need fufu or something like that
makeCoalg' :: Seed' -> TreeF Node Seed'
makeCoalg' (this, visited, g, canMakeSecondVisit)
  | this == "end" = TNode this []
  | otherwise = TNode this $ (, this:visited, g, canMakeSecondVisit && not isSecondVisit) <$> kids
  where
    visitAllowed x = not (isSmall x) || x `notElem` visited || canMakeSecondVisit
    kids = filter (/= "start") $ filter visitAllowed $ g M.! this
    isSecondVisit = isSmall this && this `elem` visited


-- a -> f (Either (Fix f) a)
makeCoalg'' :: Seed' -> TreeF (Either (Fix (TreeF Node)) Seed') Seed'
makeCoalg'' (this, visited, g, canMakeSecondVisit)
  | this == "end" = undefined --TNode this (Left [])
  | otherwise = undefined --TNode this $ (, this:visited, g, canMakeSecondVisit && not isSecondVisit) <$> kids
  where
    visitAllowed x = not (isSmall x) || x `notElem` visited || canMakeSecondVisit
    kids = filter (/= "start") $ filter visitAllowed $ g M.! this
    isSecondVisit = isSmall this && this `elem` visited



render :: Tree Node  -> [String]
render = cata renderAlg

renderAlg :: TreeF Node [String] -> [String]
renderAlg (TNode n []) = [n]
renderAlg (TNode n ss) = (\s -> n ++ ", " ++ s) <$> concat ss


paths :: Tree Node -> [[Node]]
paths = cata pathAlg


pathAlg :: TreeF Node [[Node]] -> [[Node]]
pathAlg (TNode n []) = [[n]]
pathAlg (TNode n ss) = concat $ ((n:) <$>) <$> ss

