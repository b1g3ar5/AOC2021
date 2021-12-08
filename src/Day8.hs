
module Day8 where


import Utils
import Data.List (permutations, sort, find)
import Data.Vector.Unboxed.Base (Vector(V_Bool))
import Data.Maybe 
import qualified Alg as Data


-- Here I just use brute force. There are only 5040 permuations and 200 Enrties
-- so it's comfortable and the code take about 1.5 seconds

-- Initially I wrote my own find functions. I've changed them to the stock ones in Data.List
-- but I'm not sure it improves readability


type Signal = [String]
type Output = [String]


type Entry = (Signal, Output)


parseEntry :: String -> Entry
parseEntry s = (words $ head ss, words $ ss!!1)
  where
    ss = splitOn "|" s


-- Corresponding to [0..9]
numbers :: [String]
numbers = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]


valid :: String -> Bool
valid = (`elem` numbers)

-- converts a list of digits which are the hundreds, tens and units to a number
convert :: [Int] -> Int
convert is = read $ concat $ show <$> is


solve :: Entry -> Int
solve (ss,os) = convert $ fromJust . decodeToNumber (go $ permutations "abcdefg") <$> os
  where
    go :: [String] -> String
    go ps = fromMaybe (error "None of the permutations work") 
          $ find (\p -> all (valid . decode p) ss) ps


decodeToNumber :: String -> String -> Maybe Int
decodeToNumber code ct = findTuple (zip numbers [0..]) $ decode code ct


-- Decodes to plain text (ie. the segments which are on)
-- This should really check that the resulting string is the same length
-- as ct - I am assuming there are no errors in the input...
decode :: String -> String -> String
decode code ct = sort $ catMaybes $ findTuple (zip code "abcdefg") <$> ct


-- Specialises find to tuples and takes the associated snd item
findTuple :: (Foldable t, Eq a) => t (a, b) -> a -> Maybe b
findTuple ts pattern  = snd <$> find (fst . first (== pattern)) ts


day8 :: IO ()
day8 = do
  inLines <- getLines 8
  let entries = parseEntry <$> inLines
      outputs = snd <$> entries

  putStrLn $ "Day8: part1: " ++ show (length $ filter (\o -> length o `elem` [2,4,3,7]) $ concat outputs)
  putStrLn $ "Day8: part2: " ++ show (sum $ solve <$> entries)

  return ()


