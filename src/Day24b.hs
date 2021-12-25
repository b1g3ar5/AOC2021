module Day24b where


import Utils
import qualified Data.Map.Strict as M

import Debug.Trace

data Ins = Mod Reg Var | Add Reg Var| Div Reg Var| Mul Reg Var| Eql Reg Var | Inp Reg deriving (Eq)


showIns :: Map Reg String -> Ins -> String
showIns rs (Mod r (Right 1)) = rs M.! r
showIns rs (Add r (Right 0)) = rs M.! r
showIns rs (Mul r (Right 1)) = rs M.! r
showIns rs (Mul r (Right 0)) = "0"
showIns rs (Div r (Right 1)) = rs M.! r

showIns rs (Mod r v)
  | rs M.! r == "0" = "0"
  | rs M.! r == "1" = "1"
  | otherwise = "(" ++ (rs M.! r) ++ "`mod`" ++ showVar rs v ++ ")"
showIns rs (Add r v)
  | rs M.! r == "0" = showVar rs v
  | otherwise  = "(" ++ (rs M.! r) ++ "+" ++ showVar rs v ++ ")"
showIns rs (Mul r v)
  | rs M.! r == "0" = "0"
  | rs M.! r == "1" = show v
  | otherwise  = "(" ++ (rs M.! r) ++ "*" ++ showVar rs v ++ ")"
showIns rs (Div r v)
  | rs M.! r == "1" = show v
  | otherwise  = "(" ++ (rs M.! r) ++ "/" ++ showVar rs v ++ ")"


showIns rs (Eql r (Right n)) = "(" ++ (rs M.! r) ++  "==" ++ show n ++ "?1:0)"
showIns rs (Eql r (Left v)) = "(" ++ (rs M.! r) ++  "==" ++ (rs M.! v) ++ "?1:0)"
showIns rs (Inp r) = rs M.! r


type Var = Either Reg Int

showVar :: Registers -> Var -> String
showVar rv (Left r) = rv M.! r
showVar rv (Right n) = show n

data Reg = W | X | Y | Z deriving (Eq, Show, Ord)

parseReg :: String -> Reg
parseReg "w" = W
parseReg "x" = X
parseReg "y" = Y
parseReg "z" = Z
parseReg c = error $ "Not a reg: " ++ c


type Registers = Map Reg String

getVar :: Var -> Registers -> String
getVar (Right n) rs = show n
getVar (Left v) rs = rs M.! v

set:: Reg -> String -> Registers -> Registers
--set r i rs = trace ("z: " ++ show (rs M.! Z)) $ M.insert r i rs
set = M.insert


parseVar :: String -> Var
parseVar "w" = Left W
parseVar "x" = Left X
parseVar "y" = Left Y
parseVar "z" = Left Z
parseVar n = Right $ read n


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


type Computer = ([String], [Ins], Registers)


--run :: Computer -> String
run (inp, [], rs) = rs -- M.! Z
run (inp, i@(Add a b):is, rs) = run (inp, is, set a (showIns rs i) $ pruned rs)
run (inp, i@(Mul a b):is, rs) = run (inp, is, set a (showIns rs i) $ pruned rs)
run (inp, i@(Div a b):is, rs) = run (inp, is, set a (showIns rs i) $ pruned rs)
run (inp, i@(Mod a b):is, rs) = run (inp, is, set a (showIns rs i) $ pruned rs)
run (inp, i@(Eql a b):is, rs) = run (inp, is, set a (showIns rs i) $ pruned rs)
run (inp, i@(Inp a):is, rs) = run (tail inp, is, set a (head inp) $ pruned rs)

-- ((25*(((((((n1+2)*26)+(n2+16))`mod`26)+14)==n3?1:0)==0?1:0))+1)
rng :: [Int]
rng = [9,8..0]

validNumbers :: [Ins] -> [Int]
validNumbers ins = do
  let rng :: [Int]
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
  --guard $ run (ns, ins, M.fromList [(X,0),(Y,0),(Z,0),(W,0)])
  return $ foldl1 (\acc n -> n + acc * 10) ns


pruned rs = 
            --replace "(((((((n1+2)*26)+(n2+16))*26+((n3+9)))*26+n4/26)*((25*(((((((((((n1+2)*26)+(n2+16))*26+((n3+9)))*26+n4`mod`26)-8)==n5?1:0)==0?1:0))+1))+((n5+1)*(((((((((((n1+2)*26)+(n2+16))*26+((n3+9)))*26+n4`mod`26)-8)==n5?1:0)==0?1:0)))`mod`26)+10)==n6?1:0)==0?1:0)))" "1" .
            --replace "(n4)" "n4" .
            --replace "((25+1))" "26" .
            --replace "(((((((((n1+2)*26)+(n2+16))*26+((n3+9)))`mod`26)+15)==n4?1:0)==0?1:0))" "1" .
            --replace "+-8" "-8" .
            --replace "*(1)" "" .
            --replace "*1" "" .
            --replace "((25*(1)+1))" "26" .
            --replace "((((((((n1+2)*26)+(n2+16))*26+((n3+9)*1))`mod`26)+15)==n4?1:0)==0?1:0)" "1" .
            --replace "((25*1)+1)" "26" .
            --replace "(((((((n1+2)*26)+(n2+16))`mod`26)+14)==n3?1:0)==0?1:0)" "1" .
            --replace "((n1+2)*26)" "(n1+2)*26" .
            --replace "+((n2+16))" "+n2+16" .
            --replace "((25)+1)" "26" .
            --replace "*(((n1+17)==n2?1:0)==0?1:0)" "" .
            --replace "((n1+2)*((10==n1?1:0)==0?1:0))" "(n1+2)" .
            --replace "(((n1+2)`mod`26)+15)" "(n1+17)" <$> 
            rs


day24b :: IO ()
day24b = do
  ls <- getLines 24
  let ins = parseIns <$> take 18 ls 
      input :: [String]
      input = (\n -> 'n':show n ) <$> [1..14]
      comp = (input, ins, M.fromList [(X,"0"),(Y,"0"),(Z,"0"),(W,"0")])
      output = run comp
  putStrLn $ "Day24: part1:comp: " ++ show output
  putStrLn $ "Day24: part1:comp: " ++ show (length output)

  return ()



