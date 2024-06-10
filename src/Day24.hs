module Day24 where


import Utils
import qualified Data.Map.Strict as M

-- I did this by inspecting the code and writing the algorithm in a 
-- spreadsheet with 14 ros one for each input integer...

-- other thoughts were to split the code into parts, one for each input
-- with the output registers of one part as input to the next

-- Then I thought I could parse each part as a string of code
-- and that code could be pruned to get rid of parts that had no effect...
-- and then it could be reversed as I did in the spread sheet


data Ins = Mod Reg Var | Add Reg Var| Div Reg Var| Mul Reg Var| Eql Reg Var | Inp Reg deriving (Eq)
type Var = Either Reg Int
data Reg = W | X | Y | Z deriving (Eq, Show, Ord)
type Registers = Map Reg Int
type Computer = ([Int], [Ins], Registers)


parseReg :: String -> Reg
parseReg "w" = W
parseReg "x" = X
parseReg "y" = Y
parseReg "z" = Z
parseReg c = error $ "Not a reg: " ++ c


getVar :: Var -> Registers -> Int
getVar (Right n) rs = n
getVar (Left v) rs = rs M.! v


set:: Reg -> Int -> Registers -> Registers
set = M.insert

parseVar :: String -> Var
parseVar "w" = Left W
parseVar "x" = Left X
parseVar "y" = Left Y
parseVar "z" = Left Z
parseVar n = Right $ read n


parseIns :: String -> Ins
parseIns s
  | i=="add" = Add v1 v2
  | i=="div" = Div v1 v2
  | i=="mul" = Mul v1 v2
  | i=="eql" = Eql v1 v2
  | i=="mod" = Mod v1 v2
  | i=="inp" = Inp v1
  | otherwise = error $ "No parse for: " ++ s
  where
    (i:vs) = words s
    v1 = parseReg $ vs!!0
    v2 = parseVar $ vs!!1


run :: ([Int], [Ins], Registers) -> Int
run (inp, [], rs) = rs M.! Z 
run (inp, i@(Add a b):is, rs) = run (inp, is, set a ((rs M.! a) + getVar b rs) rs)
run (inp, i@(Mul a b):is, rs) = run (inp, is, set a ((rs M.! a) * getVar b rs) rs)
run (inp, i@(Div a b):is, rs) = run (inp, is, set a ((rs M.! a) `div` getVar b rs) rs)
run (inp, i@(Mod a b):is, rs) = run (inp, is, set a ((rs M.! a) `mod` getVar b rs) rs)
run (inp, i@(Eql a b):is, rs) = run (inp, is, set a (if (rs M.! a) == getVar b rs then 1 else 0) rs)
run (inp, i@(Inp a):is, rs) = run (tail inp, is, set a (head inp) rs)


day24 :: IO ()
day24 = do
  ls <- getLines 24
  let ins = parseIns <$> ls 
      biggest, smallest :: [Int]
      biggest = [9,8,4,9,1,9,5,9,9,9,7,9,9,4]
      smallest = [6,1,1,9,1,5,1,6,1,1,1,3,2,1]
      start = M.fromList [(X,0),(Y,0),(Z,0),(W,0)]
      output x = run ([x,x], ins, start)


  putStrLn $ "Day24: part1:biggest: " ++ show (run (biggest, ins, start))
  putStrLn $ "Day24: part1:smallest: " ++ show (run (smallest, ins, start))
  putStrLn $ "Day24: part1: " ++ concat (show <$> biggest)
  putStrLn $ "Day24: part1: " ++ concat (show <$> smallest)

  return ()


rng :: [Int]
rng = [9,8..0]

validNumbers :: [Ins] -> [Int]
validNumbers ins = do
  let start = M.fromList [(X, 0),(Y, 0),(Z, 0),(W, 0)]
      rng :: [Int]
      rng = [9,8..0]
  i1 <- rng
  i2 <- rng
  i3 <- rng
  i4 <- rng
  i5 <- rng
  i6 <- rng
  i7 <- rng
  i8 <- rng
  i9 <- rng
  i10 <- rng
  i11 <- rng
  i12 <- rng
  i13 <- rng
  i14 <- rng
  let ns :: [Int]
      ns = [i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14]
  --guard $ run (ns, ins, start)
  return $ foldl1 (\acc n -> n + acc * 10) ns


parseAdds :: [String] -> [Int]
parseAdds ls = foldl go [] [15,33..249]
  where
    go acc i = acc ++ [gg (ls!!i)]
    gg :: String -> Int
    gg s = read $ words s !! 2


parseDivs :: [String] -> [Int]
parseDivs ls = foldl go [] [4,22..239]
  where
    go acc i = acc ++ [gg (ls!!i)]
    gg :: String -> Int
    gg s = read $ words s !! 2
