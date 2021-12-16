module School (School, add, empty, grade, sorted) where

import qualified Data.List as L
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap


type School = IntMap [String]

add :: Int -> String -> School -> School
add gradeNum student = IntMap.alter upsert gradeNum
  where upsert Nothing         = Just [student]
        upsert (Just existing) = Just (L.insert student existing)

empty :: School
empty = IntMap.empty

grade :: Int -> School -> [String]
grade gradeVal = (maybe [] id) . (IntMap.lookup gradeVal)

sorted :: School -> [(Int, [String])]
sorted = IntMap.assocs
