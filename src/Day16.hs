module Day16 where


import Utils hiding (toInt)
import Day5 (parseLine)


fromHex :: Char -> String
fromHex '0' = "0000"
fromHex '1' = "0001"
fromHex '2' = "0010"
fromHex '3' = "0011"
fromHex '4' = "0100"
fromHex '5' = "0101"
fromHex '6' = "0110"
fromHex '7' = "0111"
fromHex '8' = "1000"
fromHex '9' = "1001"
fromHex 'A' = "1010"
fromHex 'B' = "1011"
fromHex 'C' = "1100"
fromHex 'D' = "1101"
fromHex 'E' = "1110"
fromHex 'F' = "1111"
fromHex c = error $ "This not a hex character: " ++ [c]


data Packet = Packet {ver :: Int, typeId :: Int, payload :: Either Int [Packet]} deriving (Show, Eq)


toInt :: String -> Int
toInt [] = error "Run out of digits in toInt"
toInt [c] = if c=='1' then 1 else 0
toInt s = 2 * toInt (init s) + if last s =='1' then 1 else 0


type Parser a = String -> (a,String)


parsePacket :: Parser Packet
parsePacket s
  | length s < 6 = error $ "parsePacket: String not long enough: " ++ s
  | t==4 = (Packet v t (Left lit), rem1)
  | otherwise = (Packet v t (Right op), rem2)
  where
    v = toInt $ take 3 s
    t = toInt $ take 3 $ drop 3 s
    (lit, rem1) = parseLiteral $ drop 6 s
    (op, rem2) = parseOperator $ drop 6 s


parseLiteral :: Parser Int
parseLiteral = go []
  where
    go _ [] = error "Ran out of data in parse"
    go acc (t:ts)
      | t == '1' =  go (acc ++ take 4 ts) $ drop 4 ts
      | t == '0' = (toInt $ acc ++ take 4 ts, drop 4 ts)
      | otherwise = error "parseLiteral: Got a character other than a [0|1]"


parseOperator :: Parser [Packet]
parseOperator s
  | lenId == '0' = (subPackets2, drop (lengthOfSubpackets + 16) s)
  | lenId == '1' = (subPackets1, rem1)
  | otherwise = undefined
  where
    lenId = if null s then error "s is null in parseOperator" else head s
    numberOfSubpackets = toInt $ take 11 $ tail s
    lengthOfSubpackets = toInt $ take 15 $ tail s

    (subPackets1, rem1) = parseByNumber numberOfSubpackets [] $ drop 12 s
    (subPackets2, rem2) = parseByLength [] $ take lengthOfSubpackets $  drop 16 s

    parseByNumber :: Int -> [Packet] -> Parser [Packet]
    parseByNumber n acc s
      | n==0 = (acc, s)
      | null s = (acc, s)
      | otherwise = parseByNumber (n-1) (acc ++ [w]) t
      where
        (w,t) = parsePacket s

    parseByLength :: [Packet] -> Parser [Packet]
    parseByLength ps s
      | null s = (ps, [])
      | otherwise = parseByLength (ps ++ [w]) t
      where
        (w,t) = parsePacket s
        
     
versionTotal :: Packet -> Int
versionTotal (Packet v t (Left l)) = v
versionTotal (Packet v t (Right ps)) = v + sum (versionTotal <$> ps)
  

value :: Packet -> Int
value (Packet v t (Left l)) = l
value (Packet v t (Right ps))
  | t==0 = sum $ value <$> ps
  | t==1 = product $ value <$> ps
  | t==2 = minimum $ value <$> ps
  | t==3 = maximum $ value <$> ps
  | t==5 = if value (head ps) > value (ps!!1) then 1 else 0
  | t==6 = if value (head ps) < value (ps!!1) then 1 else 0
  | t==7 = if value (head ps) == value (ps!!1) then 1 else 0
  | otherwise = error ""


day16 :: IO ()
day16 = do
  inLines <- getLines 16
  let ls = head inLines --test7
  putStrLn $ "Day16: part1: " ++ show (versionTotal $ fst $ parsePacket $ concat $ fromHex <$> ls)
  putStrLn $ "Day16: part1: " ++ show (value $ fst $ parsePacket $ concat $ fromHex <$> ls)

  return ()
