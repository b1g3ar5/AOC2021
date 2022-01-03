# Advent Of Code 2021

Solutions to the Advent of Code 2021:

    https://adventofcode.com/2021

My solutions try to be neat and I'm hoping understandable. If I look at the code in a years time I hope to be able to understand it.

I have 3 which take longed than 5 seconds

Day20 - applying a filter to a 150x150 grid which takes 8 seconds using the Store comonad
         (see https://chrispenner.ca/posts/conways-game-of-life)

Day23 - a Dijkstra search to find the solution of a little game which takes 10 seconds

Day25 - which takes 10 seconds to update movement in a grid until iy reaches a gridlock


Comments:

Day6 - using the MultiSet - new to me and useful
Day10 - I used view patterns which are again new to me and can be neat
Day11 - another grid (of octopuses) which I might use Store next time as I did in Day20
Day12 - counting paths through a graph solved initially with a dfs and then resolved with a hylomorphism
Day18 - tree manipulation, I think this could be neatened up a lot with morphisms ?!
Day19 - so complicated that I had to write a separate code file for the frames of the scanners...
Day20 - a grid with a filter to be applied. I did 3 representations of the grid - simple map, store comonad
        and a fixed sized gris with the grids library. The grids library was complicated and slow so I
        prefer the Store comonad.
Day21 - memoisation using memoFix from the MemoTrie library
