module Counter (count, countAll, allCountsByFrequency) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

type Counter a = Map a Int

countAll :: Ord a => [a] -> Counter a
countAll = L.foldr updateCounts M.empty
  where updateCounts = M.alter $ Just . maybe 1 succ

count :: Eq a => a -> [a] -> Int
count needle = L.length . L.filter (== needle)

allCountsByFrequency :: Ord a => [a] -> [(a, Int)]
allCountsByFrequency xs = L.sortOn snd $ M.toList $ countAll xs
