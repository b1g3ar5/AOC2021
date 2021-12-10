module Search where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List ((\\))
import Utils (Coord)


-- elements that we want to label
isPossible :: k -> a -> Bool
isPossible = undefined

-- How to work out the label given a point
-- Could be just the b given...
getLabel :: Coord -> a -> b -> b
getLabel = undefined


-- How to work out the neighbours of a point
neighbours :: Coord -> [Coord]
neighbours = undefined


label :: Monoid b => Map Coord a -> Map Coord b
label heightMap = go pool M.empty
  where
    pool = M.keys $ M.filterWithKey isPossible heightMap
    go :: Monoid b => [Coord] -> Map Coord b -> Map Coord b
    go [] inMap = inMap
    go (k:ks) inMap = go rem outMap
      where
        (rem, outMap) = region [k] ks inMap

    -- params are: the pipeline, pool of possibles, current map of regions
    region :: Monoid b => [Coord] -> [Coord] -> Map Coord b -> ([Coord], Map Coord b)
    region [] possibles mp = (possibles, mp)
    region (c:cs) possibles mp = region (cs++ns) (possibles \\ ns) mpp
      where
        mpp = M.insert c (getLabel c (heightMap M.! c) mempty) mp
        ns = filter (`elem` possibles) $ neighbours c

