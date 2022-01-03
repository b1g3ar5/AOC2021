{-# Language DataKinds, FlexibleContexts, TypeFamilies #-}

module Day20c where


import Utils hiding (Coord)
import Data.Grid
import Data.Grid.Internal.Coord
import Data.Functor.Compose
import Data.Foldable


-- A version using the sized (comonadic, representable) grid, very slow...

type Code = [Bool]


parse :: [String] -> (Code, Grid '[200,200] Bool)
parse css = ((=='#') <$> head css, image)
  where
    n = 50
    image = (=='#') <$> fromNestedLists' (addBuffers (tail (tail css)))
    -- Add buffers so that the bounds don't matter...
    addBuffers :: [String] -> [String]
    addBuffers ss = topBuffer ++ (go <$> ss) ++ topBuffer
      where
        h = length ss
        w = length $ head ss
        topBuffer = replicate n (replicate (w+(2*n)) '.')
        go :: String -> String
        go s = replicate n '.' ++ s ++ replicate n '.' 


-- Make the code global so that the step needs no extra parameters
code :: Code
code = (=='#') <$> "#.#.#.#.#......#.#.#.#.##..#.##.##..#..##...#.#.#.#...##.##.##.###....#..#...#.#..###.#...#..##.#.###..#..####.###...#.#.#..##..##.##..##..###..#....#.#....#####.#...###...#.#....###...#..##.##..#..#.##..###..#.##.###..#.####...#.##.....#.###...#.##.##.#.#######...#.###..##..##..#.#.#.#####...#....#.....##.#.#...##.######....#..#......#.#.#.#.##...######.#.#####..#####..#.#.#.#.###.#.#....#..##..#..#.#.#..##....##..#.#.......##...#..####.####.#.#..#.###..#...#......###...#...#.##.#.####..#.#....###.####..#."


toInt9 :: Code -> Int
toInt9 [x1,x2,x3,x4,x5,x6,x7,x8,x9] = n x1 * 256 + n x2 * 128 + n x3 * 64 + n x4 * 32 + n x5 * 16 + n x6 * 8 + n x7 * 4 + n x8 * 2 + n x9
  where
    n b = if b then 1 else 0
toInt9 bs = error $ "We need 9 booleans: " ++ show bs


step :: (IsGrid dims) =>  Grid dims Bool -> Grid dims Bool
step = autoConvolute clampBounds val
  where
    val :: Grid '[3, 3] Bool -> Bool
    val = (code !!) . toInt9 . concat . toNestedLists


render :: (IsGrid '[x, y]) => Grid '[x, y] Bool -> String
render = intercalate "\n" . toNestedLists . fmap (bool '.' '#')


pixel :: (IsGrid '[x, y]) => Grid '[x, y] Bool -> Int
pixel = sum . (sum <$>) . toNestedLists . fmap (bool 0 1)


day20c :: IO ()
day20c = do
  ls <- getLines 20
  let (code, grid) = parse ls
  putStrLn $ "Day20c: Sized grid, part1: " ++ show (pixel $ iterate step grid !! 2)
  putStrLn $ "Day20c: Sized grid, part2: " ++ show (pixel $ iterate step grid !! 50)

  return ()


