module Lib
    ( libMain
    ) where


import System.TimeIt
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day20b
import Day20c
import Day21
import Day22
import Day23
import Day24
import Day24b
import Day25
import qualified Data.Functor as Projects
import Frames

{-
Projects...

Monadic version if Day10
Sort out a grid for Day 11 - comonad? Fixed size?
Generalise the DFS on day 12 for Utils
Day12 as a hylomorphism?

-}

libMain :: IO ()
libMain = do
  timeIt day1
  timeIt day2
  timeIt day3
  timeIt day4
  timeIt day5 
  timeIt day6
  timeIt day7
  timeIt day8
  timeIt day9
  timeIt day10
  timeIt day11
  timeIt day12
  timeIt day13
  timeIt day14
  timeIt day15  -- 5s
  timeIt day16
  timeIt day17
  timeIt day18 -- 3s
  timeIt day19
  timeIt day20 -- 8s
  --timeIt day20b
  --timeIt day20c
  timeIt day21
  timeIt day22
  timeIt day23 -- 10s
  timeIt day24
  timeIt day25 -- 10s



--Usage: sequence_ $ makeFile <$> [1..25]
makeFile :: Int -> IO ()
makeFile n = do
  let fileName = "./src/Day" ++ show n ++ ".hs"
  writeFile fileName $ template n


template :: Int -> String
template n = unlines ["module Day" ++ show n ++ " where"
  , ""
  , ""
  , "import Utils"
  , ""
  , ""
  , "day" ++ show n ++ " :: IO ()"
  , "day" ++ show n ++ " = do"
  , "  inLines <- getLines " ++ show n
  , "  putStrLn $ \"Day" ++ show n ++ ": part1: \" ++ show \"\""
  , "  putStrLn $ \"Day" ++ show n ++ ": part2: \" ++ show \"\""
  , ""
  , "  return ()"
  , ""
  ]  