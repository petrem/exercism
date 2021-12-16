module Queens (boardString, canAttack) where

import Data.List (intersperse, (\\))


type Position = (Int, Int)

isOnBoard :: Position -> Bool
isOnBoard (x, y) = and [x >= 0, x <=7, y >=0, y <= 7]


boardString :: Maybe Position -> Maybe Position -> String
boardString white black = unlines [
  intersperse ' ' [queen (r, f) | f <- [0..7]] |  r <- [0..7]]
  where
    queen :: Position -> Char
    queen pos
          | maybe False (pos ==) white = 'W'
          | maybe False (pos ==) black = 'B'
          | otherwise                  = '_'


rank :: (a, b) -> a
rank = fst

file :: (a, b) -> b
file = snd

canAttack :: Position -> Position -> Bool
canAttack queenA queenB
  | rank queenA == rank queenB = True
  | file queenA == file queenB = True
  | otherwise                  =
      absDiff (file queenA) (file queenB) == absDiff (rank queenA) (rank queenB)
  where absDiff x y = abs(x - y)

-- A more interesting way (though not efficient, obviously)

newtype AdditiveInt = I { getInt :: Int } deriving (Eq, Ord, Show)

instance Semigroup AdditiveInt where
  i1 <> i2 = I ( getInt i1 + getInt i2)

diagonAlleys :: Position -> [Position]
diagonAlleys pos = filter isOnBoard $ map getPosition [fromPosition pos <> fromPosition (i, j) | i <- range, j <- range, abs i == abs j]
  where range = [-7..7] \\ [0]
        getPosition (I x, I y) = (x, y)
        fromPosition (x, y) = (I x, I y)

canAttack' :: Position -> Position -> Bool
canAttack' queenA queenB
  | rank queenA == rank queenB        = True
  | file queenA == file queenB        = True
  | queenA `elem` diagonAlleys queenB = True
  | otherwise                         = False
