{-# LANGUAGE FlexibleInstances#-}

module Day10 where


import Utils (getLines, sort, first)


data Bracket = Paren | Square | Curly | Angle  deriving (Eq, Show)
type IsOpening = Bool


parseBracket :: Char -> (IsOpening, Bracket)
parseBracket '(' = (True, Paren)
parseBracket '[' = (True, Square)
parseBracket '{' = (True, Curly)
parseBracket '<' = (True, Angle)
parseBracket ')' = (False, Paren)
parseBracket ']' = (False, Square)
parseBracket '}' = (False, Curly)
parseBracket '>' = (False, Angle)
parseBracket c = error $ "Not a bracket: " ++ show [c]


data Parsed a = Corrupt a | Unfinished [a] | Valid deriving (Eq, Show)


isCorrupt :: Parsed a -> Bool
isCorrupt (Corrupt _) = True
isCorrupt _ = False


parseString :: String -> Parsed (IsOpening, Bracket)
parseString = go []
  where
    go [] [] = Valid
    go acc [] = Unfinished acc
    go acc (b:bs)
      | b `elem` "({[<" = go (pb:acc) bs
      | b `elem` ")]}>" = if head acc == first not pb then go (tail acc) bs else Corrupt pb
      | otherwise  = error $ "Cannot parse b: " ++ [b]
      where
        pb = parseBracket b




cost1 :: Bracket -> Int
cost1 Paren = 3
cost1 Square = 57
cost1 Curly = 1197
cost1 Angle = 25137


cost2 :: Bracket -> Int
cost2 Paren = 1
cost2 Square = 2
cost2 Curly = 3
cost2 Angle = 4


-- View patterns!! Use these if you immediately pattern math on a call to a function
-- See https://blog.ocharles.org.uk/posts/2014-12-02-view-patterns.html
score1 :: String -> Int
score1 (parseString -> Corrupt b) = cost1 $ snd b
score1 _ = 0


score2 :: String -> Int
score2 (parseString -> Unfinished t) = go 0 $ snd <$> t
  where
    go n [] = n
    go n (c:cs) = go (5*n + cost2 c) cs
score2 _ = 0


day10 :: IO ()
day10 = do
  ls <- getLines 10
  let ps = sort $ filter (/=0) $ score2 <$> ls
      ps' = sort $ filter (/=0) $ score2' <$> ls
      n2 = length ps `div` 2

  putStrLn $ "Day10: part1: " ++ show (sum $ score1 <$> ls)
  putStrLn $ "Day10: part1: " ++ show (sum $ score1' <$> ls)
  putStrLn $ "Day10: part2: " ++ show (ps !! n2)
  putStrLn $ "Day10: part2: " ++ show (ps' !! n2)

  return ()



op :: Char -> Char
op '(' = ')'
op '[' = ']'
op '{' = '}'
op '<' = '>'
op ')' = '('
op ']' = '['
op '}' = '{'
op '>' = '<'
op _ = error "Not bracket"


parseString' :: String -> [Char]
parseString' = go []
  where
    go [] [] = []
    go acc [] = acc
    go acc (b:bs)
      | b `elem` "({[<" = go (b:acc) bs
      | b `elem` ")]}>" = if head acc == op b then go (tail acc) bs else [b]
      | otherwise  = error $ "Cannot parse b: " ++ [b]


score1' :: String -> Int
score1' (parseString' -> [b]) = cost1' b
score1' _ = 0


cost1' :: Char -> Int
cost1' ')' = 3
cost1' ']' = 57
cost1' '}' = 1197
cost1' '>' = 25137
cost1' _ = error "Not a bracket"


score2' :: String -> Int
score2' (parseString' -> t) = if length t == 1 then 0 else go 0 t
  where
    go n [] = n
    go n (c:cs) = go (5*n + cost2' c) cs


cost2' :: Char -> Int
cost2' '(' = 1
cost2' '[' = 2
cost2' '{' = 3
cost2' '<' = 4
cost2' c = error $ "Not a bracket" ++ [c]


