module Day18 where


import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Data.Void
import Debug.Trace

type Parser = Parsec Void String

type Level = Int 

-- A tree, nodes labelled with the depth
data Tree = Leaf (Level, Int) | Node Level Tree Tree deriving (Show, Eq)
isLeaf :: Tree -> Bool 
isLeaf (Leaf _) = True
isLeaf _ = False

getInt :: Tree -> Int
getInt (Leaf (l,x)) = x
getInt t = error $ "can't getInt from: " ++ showTree t


addR, addL :: Int -> Tree -> Tree
addR x (Leaf (lab,y)) = Leaf (lab, x+y)
addR x (Node n lt rt) = Node n lt (addR x rt)
addL x (Leaf (lab,y)) = Leaf (lab, x+y)
addL x (Node n lt rt) = Node n (addL x lt) rt 


increment :: Int -> Tree -> Tree
increment n (Leaf (m, x)) = Leaf (n+m, x)
increment n (Node m l r) = Node (n+m) (increment n l) (increment n r)

showTree :: Tree -> String
showTree (Leaf (n,x)) = show x
showTree (Node n l r) = "[" ++ showTree l ++ "," ++ showTree r ++ "]"

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
  return $ Node n x y


parseElement :: Level -> Parser Tree 
parseElement n = ((\i -> Leaf (n, i)) <$> parseInt) <|> parsePair n


explode :: Tree -> (Bool, Int, Int, Tree)
explode (Leaf x) = (False, 0, 0, Leaf x)
explode node@(Node lvl l r)
  | lvl>3 && isLeaf l && isLeaf r = (True, getInt l, getInt r, Leaf (lvl,0)) 
  | lvl>3 && isLeaf l = (True, 0, ry, Node lvl (addR rx l) rt) -- explode the right
  | lvl>3 = (True, lx, 0, Node lvl lt (addL ly rt)) -- explode the left
  | otherwise = 
      if el then
        (True, lx, 0, Node lvl lt (addL ly r))
      else        
        if er then
          (True, 0, ry, Node lvl (addR rx l) rt)
        else
          (False, 0, 0, Node lvl l r)
  where
    (el, lx, ly, lt) = explode l        
    (er, rx, ry, rt) = explode r


split :: Tree -> Tree
split (Node n l r) = Node n sl (if sl == l then split r else r)
  where
    sl = split l
split (Leaf (n, x)) = if x > 9 then Node n (Leaf (n+1, d2)) (Leaf (n+1, d2 + if even x then 0 else 1)) else Leaf (n, x)
  where
    d2 = x `div` 2


addTree :: Tree -> Tree -> Tree
addTree l r = reduce s
  where
    s = Node 0 (increment 1 l) (increment 1 r)
    

reduce :: Tree -> Tree
reduce = steadyState (split . steadyState (t4 . explode))
  where
    t4 (_,_,_,x) = x


magnitude :: Tree -> Int
magnitude (Leaf (n, x)) = x
magnitude (Node _ l r) = 3 * magnitude l + 2 * magnitude r


maxMagnitude :: [Tree] -> Int
maxMagnitude = maximum . go 
  where
    go :: [Tree] -> [Int]
    go ts = do
      t1 <- ts
      t2 <- ts
      guard (t1 /= t2)
      return $ max (magnitude $ addTree t1 t2) (magnitude $ addTree t2 t1)
    

day18 :: IO ()
day18 = do
  ls <- getLines 18
  let ns = catMaybes $ parseMaybe (parsePair 0) <$> ls

  putStrLn $ "Day18: part2: " ++ show (magnitude $ foldl1 addTree ns) 
  putStrLn $ "Day18: part2: " ++ show (maxMagnitude ns)


  return ()

