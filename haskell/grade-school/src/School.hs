module School (School, add, empty, grade, sorted) where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (insert)
import Data.Maybe (fromMaybe)

type School = IntMap [String]

add :: Int -> String -> School -> School
add gradeNum student = IntMap.alter upsert gradeNum
  where
    upsert Nothing = Just [student]
    upsert (Just existing) = Just (insert student existing)

empty :: School
empty = IntMap.empty

grade :: Int -> School -> [String]
grade gradeVal = fromMaybe [] . IntMap.lookup gradeVal

sorted :: School -> [(Int, [String])]
sorted = IntMap.assocs
