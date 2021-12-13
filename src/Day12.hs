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
  let graph = mkGraph $ parseEdge <$> ls
  putStrLn $ "Day12: part1: " ++ show (length $ generatePaths True "start" "end" graph )
  putStrLn $ "Day12: part2: " ++ show (length $ generatePaths False "start" "end" graph)

  let t = mkTree graph  
  putStrLn $ "Tree: " ++ printTree t
  --putStrLn $ "Tree: " ++ show (paths t)

  return ()


newtype Fix f = Fix { unFix :: f (Fix f) } 
type Algebra f a = f a -> a
type Coalgebra f a = (a -> f a)
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g
fix :: (a -> a) -> a
fix f = let {x = f x} in x

data ListF a x = NilF | ConsF a x deriving (Show, Functor)
type List a = Fix (ListF a)


lengthAlgebra :: ListF a Int -> Int
lengthAlgebra NilF        = 0
lengthAlgebra (ConsF _ n) = n + 1


coAlg :: Coalgebra (ListF Int) [Int] -- [Int] -> ListF Int [Int]
coAlg [] = NilF
coAlg (n:ns) = ConsF n ns -- Here the ns is an [Int] because it's a list


algMain :: IO ()
algMain = do
  let ls = [1,2,3,4,5,6]
      p = ana coAlg ls
  putStrLn $ "p: " ++ show (cata lengthAlgebra p)
  putStrLn $ "p: " ++ show (hylo lengthAlgebra coAlg ls)


generatePaths' :: Node -> Node -> Graph -> [[Node]]
generatePaths' start end g = map reverse $ dfs [] [[]] start
  where
    dfs :: [Node] -> [[Node]] -> Node -> [[Node]]
    dfs visited acc next
      | next == end = map (next :) acc -- add end to all the paths
      | next == start && not (null visited) = []  -- don't go back to the start
      | isSecondVisit = [] -- second visit is not allowed or has already occurred
      | otherwise = concatMap (dfs (next:visited) (map (next :) acc)) $ g M.! next
      where
        isSecondVisit = isSmall next && next `elem` visited


type Seed = (Node, [Node])
data TreeF a r = TNil | TNode a [r] deriving (Functor, Eq, Show)
type Tree a = Fix (TreeF a)


mkTree :: Graph -> Tree Node
mkTree g = ana coalg ("start", [])
  where
    coalg :: Seed -> TreeF Node Seed
    coalg (next, visited)
      | next == "end" = TNode next []
      | next == "start" && not (null visited) = TNil
      | isSecondVisit = TNil
      | otherwise = TNode next ( (, next:visited) <$> g M.! next)
      where
        isSecondVisit = isSmall next && next `elem` visited


printTree :: Tree Node  -> String
printTree = cata alg
  where
    alg :: TreeF Node String -> String
    alg TNil = "\n"
    alg (TNode n ss) = concat $ (\s -> n ++ ", " ++ s) <$> ss

paths :: Tree Node -> Int
paths = cata alg
  where
    alg :: TreeF Node Int -> Int
    alg TNil = 1
    alg (TNode n ss) = sum ss
