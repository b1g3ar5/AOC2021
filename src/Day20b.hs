module Day20b where


import Utils ( Coord, Map, fromMaybe, getLines )
import qualified Data.Map.Strict as M


-- Simple but slow...

type Image = (Bool, Map Coord Bool)
type Code = [Bool]


bounds :: Map Coord a -> ((Int, Int),(Int, Int))
bounds mp = ((xmin, ymin), (xmax, ymax))
  where
    ks = M.keys mp
    xmin = minimum $ fst <$> ks
    xmax = maximum $ fst <$> ks
    ymin = minimum $ snd <$> ks
    ymax = maximum $ snd <$> ks


-- With a margin of 3 each side
toKeys :: Map Coord a -> [Coord]
toKeys mp = concat $ (\y -> (y,) <$> [(xmin-3)..(xmax+3)]) <$> [(ymin-3)..(ymax+3)]
  where
    ((xmin, ymin), (xmax, ymax)) = bounds mp  


render :: Image -> String
render (_, mp) = unlines $ (\y -> (\x -> if mp M.! (x,y) then '#' else '.') <$> [minx..maxx]) <$> [miny..maxy]
  where
    ks = M.keys mp
    minx = minimum $ fst <$> ks
    maxx = maximum $ fst <$> ks
    miny = minimum $ snd <$> ks
    maxy = maximum $ snd <$> ks


pixels :: Image -> Int 
pixels (_, mp) = length $ filter id $ M.elems mp


parse :: [String] -> (Code, Image)
parse css = ((=='#') <$> head css, (False, image))
  where
    image = M.fromList $ concat $ (\(y,cs) -> (\(x,c) -> ((x,y), c=='#')) <$> zip [0..] cs) <$> zip [0..] (tail (tail css))


neighbourhood :: Coord -> [Coord]
neighbourhood (x,y) = [(i,j) | j<-[y-1,y,y+1], i<-[x-1,x,x+1]]


enhance :: Code -> Image -> Image
enhance !lcode (!d, !lmp) = (lcode !! toInt9 (replicate 9 d), M.fromList (go <$> toKeys lmp))
  where
    go :: Coord -> (Coord, Bool)
    go pos = (pos, lcode !! ix)
      where
        neighbours = neighbourhood pos
        ix = toInt9 $ (\n -> M.findWithDefault d n lmp) <$> neighbours


toInt9 :: [Bool] -> Int
toInt9 [x1,x2,x3,x4,x5,x6,x7,x8,x9] = n x1 * 256 + n x2 * 128 + n x3 * 64 + n x4 * 32 + n x5 * 16 + n x6 * 8 + n x7 * 4 + n x8 * 2 + n x9
  where
    n b = if b then 1 else 0
toInt9 bs = error $ "We need 9 booleans: " ++ show bs


day20b :: IO ()
day20b = do
  ls <- getLines 20
  let (code, image@(_, mp)) = parse ls
      enhancedImage = iterate (enhance code) image !! 2
  putStrLn $ "Day20b: Map, part1: " ++ show (pixels $ iterate (enhance code) image !! 2)
  putStrLn $ "Day20b: Map, part2: " ++ show (pixels $ iterate (enhance code) image !! 50)

  return ()




