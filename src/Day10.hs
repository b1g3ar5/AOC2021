
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


data ParseOutcome a = Corrupt a | Unfinished [a] | Valid deriving (Eq, Show)


isCorrupt :: ParseOutcome a -> Bool
isCorrupt (Corrupt _) = True
isCorrupt _ = False


parseString :: String -> ParseOutcome (IsOpening, Bracket)
parseString = go []
  where
    go [] [] = Valid
    go acc [] = Unfinished acc
    go acc (b:bs)
      | b `elem` "({[<" = go (parseBracket b:acc) bs
      | b `elem` ")]}>" = if head acc == first not pb then go (tail acc) bs else Corrupt (parseBracket b)
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
      n2 = length ps `div` 2

  putStrLn $ "Day10: part1: " ++ show (sum $ score1 <$> ls)
  putStrLn $ "Day10: part2: " ++ show (ps !! n2)

  return ()


