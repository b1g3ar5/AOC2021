{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}

module Day20 where

import Utils (getLines, bool)
import Data.Functor.Compose (Compose(..))
import qualified Data.Vector as V
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))


-- Using thw Store comonad - 8s, still quite slow

newtype VBounded a = VBounded (V.Vector a) deriving (Eq, Show, Functor, Foldable)


instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 200

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc

type Grid a = Store (Compose VBounded VBounded) a
type Coord = (Int, Int)
type Code = [Bool]

code :: Code
code = (=='#') <$> "#.#.#.#.#......#.#.#.#.##..#.##.##..#..##...#.#.#.#...##.##.##.###....#..#...#.#..###.#...#..##.#.###..#..####.###...#.#.#..##..##.##..##..###..#....#.#....#####.#...###...#.#....###...#..##.##..#..#.##..###..#.##.###..#.####...#.##.....#.###...#.##.##.#.#######...#.###..##..##..#.#.#.#####...#....#.....##.#.#...##.######....#..#......#.#.#.#.##...######.#.#####..#####..#.#.#.#.###.#.#....#..##..#..#.#.#..##....##..#.#.......##...#..####.####.#.#..#.###..#...#......###...#...#.##.#.####..#.#....###.####..#."


parse :: [String] -> Grid Bool
parse css = mkGrid image
  where
    offset = 50
    image = fst <$> filter snd (concat $ (\(y,cs) -> (\(x,c) -> ((y+offset,x+offset), c=='#')) <$> zip [0..] cs) <$> zip [0..] (tail (tail css)))


mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup (0, 0)
  where
    lookup crd = crd `elem` xs


type Rule = Grid Bool -> Bool


neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x+dx, y+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]


basicRule :: Rule
basicRule g = code !! n
  where
    n = toInt9 $ experiment neighbours g


toInt9 :: Code -> Int
toInt9 [x1,x2,x3,x4,x5,x6,x7,x8,x9] = n x1 * 256 + n x2 * 128 + n x3 * 64 + n x4 * 32 + n x5 * 16 + n x6 * 8 + n x7 * 4 + n x8 * 2 + n x9
  where
    n b = if b then 1 else 0
toInt9 bs = error $ "We need 9 booleans: " ++ show bs


pixels :: Grid Bool -> Int
pixels (StoreT (Identity (Compose g)) _) =  sum $ sum . (bool 0 1 <$>) <$>  g


day20 :: IO ()
day20 = do
  ls <- getLines 20
  let grid = parse ls

  putStrLn $ "Day20: Store comonad, part1: " ++ show (pixels $ iterate (extend basicRule) grid !! 2)
  putStrLn $ "Day20: Store comonad, part2: " ++ show (pixels $ iterate (extend basicRule) grid !! 50)

  return ()


