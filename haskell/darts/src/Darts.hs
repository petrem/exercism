module Darts (score) where

import Control.Applicative (asum)
import Data.Maybe (fromMaybe)

score :: Float -> Float -> Int
score = score2

type Radius = Float

type Score = Int

distanceToCenter :: Float -> Float -> Float
distanceToCenter x y = sqrt $ x ^ 2 + y ^ 2

-- Solution 1

score1 :: Float -> Float -> Int
score1 x y = snd . head . filter (isInsideCircle dist) $ scores1
  where
    dist = distanceToCenter x y

isInsideCircle :: Float -> (Radius, Score) -> Bool
isInsideCircle d (r, _) = d <= r

scores1 :: [(Radius, Score)]
scores1 =
  [ (1, 10),
    (5, 5),
    (10, 1),
    (floatInfinity, 0)
  ]

floatInfinity :: Float
floatInfinity = read "Infinity"

-- Solution 2

score2 :: Float -> Float -> Int
score2 x y = fromMaybe 0 . asum . fmap (scoreMaybe dist) $ scores2
  where
    dist = distanceToCenter x y

scoreMaybe :: Radius -> (Radius, Score) -> Maybe Score
scoreMaybe d (r, s)
  | d <= r = Just s
  | otherwise = Nothing

scores2 :: [(Radius, Score)]
scores2 =
  [ (1, 10),
    (5, 5),
    (10, 1)
  ]
