module Yacht (yacht, Category(..)) where

import Data.List (sort)

import Counter (count, allCountsByFrequency)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht
              deriving (Eq, Enum, Show)

type DiceFace = Int
type Dice = [DiceFace]
type Score = Int

yacht :: Category -> Dice -> Score

yacht Yacht dice = 10 * computeNofAKind 5 dice

yacht FullHouse dice = if isFullHouse then sum dice else 0
  where isFullHouse = map snd (allCountsByFrequency dice) == [2, 3]

yacht FourOfAKind dice = 4 * computeNofAKind 4 dice

yacht LittleStraight dice = streightScoreFrom 1 dice

yacht BigStraight dice = streightScoreFrom 2 dice

yacht Choice dice = sum dice

yacht category dice
  | category `elem` [(Ones)..Sixes] = computeOnesToSixes category dice
  | otherwise = 0

computeOnesToSixes :: Category -> Dice -> Score
computeOnesToSixes category dice = (*) <$> id <*> flip count dice $ fromEnum category + 1

computeNofAKind :: Int -> Dice -> Score
computeNofAKind n dice = score_for_n $ last . allCountsByFrequency $ dice
  where score_for_n (which, how_many)
          | how_many >= n = which
          | otherwise     = 0

streightScoreFrom :: Int -> Dice -> Score
streightScoreFrom n dice
  | sort dice == [n .. n + 4] = 30
  | otherwise = 0
