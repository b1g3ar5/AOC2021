{-# LANGUAGE FlexibleInstances #-}

module Day18b where


import Utils (fromJust, isJust, getLines, second)
import Control.Arrow  ((<<<), (>>>), (&&&))
import Control.Applicative (Alternative((<|>)))


-- A version (partly) using anamorphism to make the tree and
-- catamorphism and paramorphism to tear it down

-- makeTree = anamorphism
-- split, explode = paramorphism (so that one side recurses and the other shortcuts)
-- show, maxumum = catamorphism

-- I was hoping to use hylomorhism, but I couldn't work out how to do reduce (and therefore add) which applies 
-- explode until it gets to a steady state etc.


type Algebra f a = f a -> a
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata f = unFix >>> fmap (cata f) >>> f
type Coalgebra f a = a -> f a
ana :: (Functor f) => Coalgebra f a -> a -> Fix f
ana f = Fix <<< fmap (ana f) <<< f
type RAlgebra f a = f (Fix f, a) -> a
para :: Functor f => RAlgebra f a -> Fix f -> a
para f = unFix >>> fmap (id &&& para f) >>> f
newtype Fix f = Fix { unFix :: f (Fix f) }


type Level = Int 
data TreeF r = Leaf (Int, Int) | Tree Int r r deriving (Show, Functor, Eq)
type Tree = Fix TreeF


instance Eq Tree where
  (Fix t1) == (Fix t2) = t1 == t2


instance Show Tree where
  show (Fix t1)  = "Fix " ++ show t1


isLeaf :: Tree -> Bool 
isLeaf (Fix (Leaf _)) = True
isLeaf _ = False


getInt :: Tree -> Int
getInt (Fix (Leaf (l,x))) = x
getInt t = error $ "can't getInt from the tree: " ++ showTree t


addL, addR :: Int -> Fix TreeF -> Tree
addL x = para alg
  where
    -- The fst tree is short circuit the snd is recurse into
    alg :: TreeF (Tree, Tree) -> Tree
    alg (Leaf (lab,y)) = Fix $ Leaf (lab, x+y)
    alg (Tree n (_, recurseIntoLeft) (shortCircuitRight, _)) = Fix $ Tree n recurseIntoLeft shortCircuitRight
addR x = para alg
  where
    -- The fst tree is short circuit the snd is recurse into
    alg :: TreeF (Tree, Tree) -> Tree
    alg (Leaf (lab,y)) = Fix $ Leaf (lab, x+y)
    alg (Tree n (shortCircuitLeft, _) (_, recurseIntoRight)) = Fix $ Tree n shortCircuitLeft recurseIntoRight


increment :: Int -> Tree -> Tree
increment n  = cata alg
  where
    alg :: TreeF Tree -> Tree
    alg (Tree m l r) = Fix $ Tree (n+m) l r
    alg (Leaf (m, x)) = Fix $ Leaf (n+m, x)


showTree :: Tree -> String
showTree = cata alg
  where 
    alg :: TreeF String -> String
    alg (Leaf (n,x)) = show x
    alg (Tree n l r) = "[" ++ l ++ "," ++ r ++ "]"


explode :: Tree -> Maybe Tree
explode t = fst <$> para explodeAlg t


explodeAlg :: TreeF (Tree, Maybe (Tree, (Int, Int))) -> Maybe (Tree, (Int, Int))
explodeAlg (Leaf x) = Nothing
explodeAlg (Tree lvl (shortCircuitLeft, mrl) (shortCircuitRight, mrr))
  | lvl>3 && isLeaf shortCircuitLeft && isLeaf shortCircuitRight = Just (Fix $ Leaf (lvl,0), (getInt shortCircuitLeft, getInt shortCircuitRight)) 
  | lvl>3 && isLeaf shortCircuitLeft = Just (Fix $ Tree lvl (addR rl shortCircuitLeft) rt, (0, rr)) -- explode the right
  | lvl>3 = Just (Fix $ Tree lvl lt (addL lr rt), (ll, 0)) -- explode the left
  | isJust mrl = Just (Fix $ Tree lvl lt (addL lr shortCircuitRight), (ll, 0))
  | isJust mrr = Just (Fix $ Tree lvl (addR rl shortCircuitLeft) rt, (0, rr))
  | otherwise = Nothing
  where
    (lt, (ll, lr)) = fromJust mrl       
    (rt, (rl, rr)) = fromJust mrr      


split :: Tree -> Maybe Tree
split = para splitAlg


splitAlg :: TreeF (Tree, Maybe Tree) -> Maybe Tree
splitAlg (Leaf (n,x)) = if x > 9 then Just $ Fix $ Tree n (Fix $ Leaf (n+1, d2)) (Fix $ Leaf (n+1, d2 + if even x then 0 else 1)) else Nothing
  where
    d2 = x `div` 2
splitAlg (Tree n (l, msl) (r, msr))
  | isJust msl = Just $ Fix $ Tree n (fromJust msl) r
  | isJust msr = Just $ Fix $ Tree n l (fromJust msr)
  | otherwise = Nothing


explodeOrSplit :: Tree -> Maybe Tree
explodeOrSplit t = fst <$> para explodeAlg t <|> para splitAlg t


reduce :: Tree -> Tree
reduce t = maybe t reduce (fst <$> para explodeAlg t <|> para splitAlg t)


addTree :: Tree -> Tree -> Tree
addTree l r = reduce $ Fix $ Tree 0 (increment 1 l) (increment 1 r)
 

magnitude :: Tree -> Int
magnitude = cata magnitudeAlg


magnitudeAlg :: TreeF Int -> Int
magnitudeAlg  (Leaf (n, x)) = x
magnitudeAlg (Tree _ l r) = 3 * l + 2 * r


maxMagnitude :: [Tree] -> Int
maxMagnitude ts = maximum $ [max (magnitude $ addTree f1 f2) (magnitude $ addTree f2 f1) | f1 <- ts, f2 <- ts, f1 /= f2]
    

-- Read the file with an anamorphism - I like this.
makeTree :: String -> Tree
makeTree s = ana makeCoalg (0, s)

makeCoalg :: (Level, String) -> TreeF (Level, String)
makeCoalg (_, []) = error "Parse error"
makeCoalg (n, [c]) = Leaf (n, read [c])
makeCoalg (n, '[':cs) = Tree n (n+1, ls) (n+1, rs)
  where
    -- Split the string into a 'seed' strings for the left and right children
    -- ie. find the comma
    (ls, rs) = goSplit 1 "" cs
    goSplit 0 acc rem = error "Got to the closing ']' before the comma in goSplit"
    goSplit n acc [] = error "Ran out of string in goSplit"
    goSplit 1 acc (',':rem) = (acc, init rem) -- found the comma, take the final ']' off rs
    goSplit n acc ('[':rem) = goSplit (n+1) (acc ++ "[") rem -- increase depth
    goSplit n acc (']':rem) = goSplit (n-1) (acc ++ "]") rem -- decrease depth
    goSplit n acc (r:rem) = goSplit n (acc ++ [r]) rem
makeCoalg (n, cs) = error $ "Error, we should have a sinlge number or an opening ['. n: " ++ show n ++ " and cs: " ++ cs


day18b :: IO ()
day18b = do
  ls <- getLines 18
  let ps = makeTree <$> ls

  putStrLn $ "Day18b: part1: Algebra version: " ++ show (magnitude $ foldl1 addTree ps)
  putStrLn $ "Day18b: part2: Algebra version: " ++ show (maxMagnitude ps)
  return ()

