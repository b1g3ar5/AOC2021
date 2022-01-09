{-# LANGUAGE FlexibleInstances #-}

module Day18 where


import Utils (catMaybes, fromJust, isJust, getLines)
import Text.Megaparsec ((<|>), Parsec, parseMaybe, single, some)
import Text.Megaparsec.Char (digitChar)
import Data.Void (Void)

type Parser = Parsec Void String
type Level = Int 
data Tree = Leaf (Int, Int) | Tree Int Tree Tree deriving (Show, Eq)


isLeaf :: Tree -> Bool 
isLeaf (Leaf _) = True
isLeaf _ = False


getInt :: Tree -> Int
getInt (Leaf (l,x)) = x
getInt t = error $ "can't getInt from the tree: " -- ++ showTree t


addR, addL :: Int -> Tree -> Tree
addR x (Leaf (lab,y)) = Leaf (lab, x+y)
addR x (Tree n lt rt) = Tree n lt (addR x rt)
addL x (Leaf (lab,y)) = Leaf (lab, x+y)
addL x (Tree n lt rt) = Tree n (addL x lt) rt 


increment :: Int -> Tree -> Tree
increment n (Leaf (m, x)) = Leaf (n+m, x)
increment n (Tree m l r) = Tree (n+m) (increment n l) (increment n r)


showTree :: Tree -> String
showTree (Leaf (n,x)) = show x
showTree (Tree n l r) = "[" ++ showTree l ++ "," ++ showTree r ++ "]"


parseInt :: Parser Int
parseInt = do
  cs <- some digitChar 
  return $ read cs


parsePair :: Int -> Parser Tree
parsePair n = do
  single '['
  x <- parseElement (n+1)
  single ','
  y <- parseElement (n+1)
  single ']'
  return $ Tree n x y


parseElement :: Level -> Parser Tree
parseElement n = ((\i -> Leaf (n, i)) <$> parseInt) <|> parsePair n


explode :: Tree -> Maybe (Tree, (Int, Int))
explode (Leaf x) = Nothing
explode node@(Tree lvl l r)
  | lvl>3 && isLeaf l && isLeaf r = Just (Leaf (lvl,0), (getInt l, getInt r)) 
  | lvl>3 && isLeaf l = Just (Tree lvl (addR rl l) rt, (0, rr)) -- explode the right
  | lvl>3 = Just (Tree lvl lt (addL lr rt), (ll, 0)) -- explode the left
  | isJust mel = Just (Tree lvl lt (addL lr r), (ll, 0))
  | isJust mer = Just (Tree lvl (addR rl l) rt, (0, rr))
  | otherwise = Nothing
  where
    mel = explode l 
    (lt, (ll, lr)) = fromJust mel       
    mer = explode r
    (rt, (rl, rr)) = fromJust mer      
    

split :: Tree  -> Maybe Tree
split (Tree n l r)
  | isJust msl = Just $ Tree n (fromJust msl) r
  | isJust msr = Just $ Tree n l (fromJust msr)
  | otherwise = Nothing
  where
    msl = split l
    msr = split r
split (Leaf (n, x)) = if x > 9 then  Just $ Tree n (Leaf (n+1, d2)) (Leaf (n+1, d2 + if even x then 0 else 1)) else Nothing
  where
    d2 = x `div` 2


reduce :: Tree -> Tree
reduce t = maybe t reduce (fst <$> explode t <|> split t)


addTree :: Tree -> Tree -> Tree
addTree l r = reduce $ Tree 0 (increment 1 l) (increment 1 r)
    

magnitude :: Tree -> Int
magnitude (Leaf (n, x)) = x
magnitude (Tree _ l r) = 3 * magnitude l + 2 * magnitude r


maxMagnitude :: [Tree] -> Int
maxMagnitude ts = maximum $ [max (magnitude $ addTree t1 t2) (magnitude $ addTree t2 t1) | t1 <- ts, t2 <- ts, t1 /= t2]
    

day18 :: IO ()
day18 = do
  ls <- getLines 18
  let ns = catMaybes $ parseMaybe (parsePair 0) <$> ls

  putStrLn $ "Day18: part2: " ++ show (magnitude $ foldl1 addTree ns) 
  putStrLn $ "Day18: part2: " ++ show (maxMagnitude ns)
  return ()


