-- should be possible to dramatically improve with "standard"/Vermeulen polynomials
-- see:
-- @article{leavens19923x+,
--   title={3x+ 1 search programs},
--   author={Leavens, Gary T and Vermeulen, Mike},
--   journal={Computers \& Mathematics with Applications},
--   volume={24},
--   number={11},
--   pages={79--99},
--   year={1992},
--   publisher={Elsevier}
-- }

module CollatzConjecture (collatz) where

import Data.Bits (countTrailingZeros, shiftR)

type Steps = Int

collatz :: Integer -> Maybe Integer
collatz x
  | x <= 0 = Nothing
  | otherwise = Just . toInteger . collatzOptimized . fromInteger $ x

collatzOptimized :: Int -> Int
collatzOptimized x
  | even x = let (steps, oddX) = makeOdd x in steps + hailstoneSteps oddX
  | otherwise = hailstoneSteps x

-- iterate hailstone until and including the result is 1
hailstoneSteps :: Int -> Int
hailstoneSteps x = foldl (\acc (s, _) -> acc + s) 0 stepsUntilOne
  where stepsUntilOne = tail $ takeWhileIncluding ((>1) . snd) steps
        steps = iterate (\(_, r) -> hailstone r) (1, x)

-- hailstone version that takes several steps
-- result is always odd and argument must also be odd

hailstone :: Int -> (Steps, Int)
hailstone x = (\(s,r) -> (s+1, r)) $ makeOdd (x * 3 + 1)

makeOdd :: Int -> (Steps, Int)
makeOdd x = let ctz = countTrailingZeros x in (ctz, shiftR x ctz)

takeWhileIncluding :: (a -> Bool) -> [a] -> [a]
takeWhileIncluding predicate = let behead [] = [] ; behead (x:_) = [x] in
                                 ((++) <$> fst <*> behead . snd ) . span predicate
