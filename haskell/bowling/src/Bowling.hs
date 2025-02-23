-- turned out rather ugly, I'll have to redo from scratch
{-# LANGUAGE StrictData #-}

module Bowling (score, BowlingError(..)) where

import Control.Monad (foldM, (<=<))

data BowlingError
  = IncompleteGame
  | InvalidRoll {rollIndex :: Int, rollValue :: Int}
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score = fmap (snd . scoreFold . fmap frameKind) . (checkComplete <=< foldM roll [])

type Roll = Int

type RollIndex = Int

type FrameCount = Int

data FrameKind
  = Strike
  | Spare Roll Roll
  | Open Roll Roll
  | Incomplete Roll
  | Filler1 Roll
  | Filler2 Roll Roll
  | Filler2' Roll
  deriving (Eq)

instance Show FrameKind where
  show Strike = "X"
  show (Spare r _) = show r ++ "/"
  show (Open r r') = show r ++ " " ++ show r'
  show (Incomplete r) = show r
  show (Filler1 r) = "(" ++ show r ++ ")"
  show (Filler2 r r') = "(" ++ show r ++ " " ++ show r' ++ ")"
  show (Filler2' r) = "(" ++ show r ++ " _)"

data Frame = Frame FrameKind FrameCount RollIndex deriving (Show)

frameKind :: Frame -> FrameKind
frameKind (Frame f _ _) = f

roll :: [Frame] -> Roll -> Either BowlingError [Frame]
-- look for bad roll
roll [] r
  | r < 0 || r > 10 = Left $ InvalidRoll 0 r
roll ((Frame _ _ idx) : _) r
  | r < 0 || r > 10 = Left $ InvalidRoll (idx + 1) r
-- at the start, we can get a strike, or the next throw will determine the frame type
roll fs@[] 10 = Right $ Frame Strike 1 0 : fs
roll fs@[] r = Right $ Frame (Incomplete r) 1 0 : fs
-- determine frame type if previous roll wasn't a strike
roll ((Frame (Incomplete r') c idx) : fs') r
  | r + r' == 10 = Right $ Frame (Spare r' r) c (idx + 1) : fs'
  | r + r' < 10 = Right $ Frame (Open r' r) c (idx + 1) : fs'
  | otherwise = Left $ InvalidRoll (idx + 1) r
-- if last frame is a strike or a spare, we get the a special fill frame
roll fs@((Frame Strike 10 idx) : _) r
  | r <= 10 = Right $ Frame (Filler2' r) 11 (idx + 1) : fs
  | otherwise = Left $ InvalidRoll (idx + 1) r
roll ((Frame (Filler2' r') 11 idx) : fs') r
  | r <= 10 && (r' == 10 || r' + r <= 10) = Right $ Frame (Filler2 r' r) 11 (idx + 1) : fs'
  | otherwise = Left $ InvalidRoll (idx + 1) r
roll fs@((Frame (Spare _ _) 10 idx) : _) r = Right $ Frame (Filler1 r) 11 (idx + 1) : fs
-- following after last frame when not a strike or spare or
-- after fillers
roll ((Frame _ c idx) : _) r | c >= 10 = Left $ InvalidRoll (idx + 1) r
-- not the start frame, not incomplete: a new frame follows
roll fs@((Frame _ c idx) : _) 10 = Right $ Frame Strike (c + 1) (idx + 1) : fs
roll fs@((Frame _ c idx) : _) r = Right $ Frame (Incomplete r) (c + 1) (idx + 1) : fs

-- assume list of Frames is reversed (as comes out of foldM roll)
-- and short (so it is not an overhead to use length)
checkComplete :: [Frame] -> Either BowlingError [Frame]
checkComplete [] = Left IncompleteGame
checkComplete ((Frame (Incomplete _) _ _) : _) = Left IncompleteGame
checkComplete ((Frame Strike _ _) : _) = Left IncompleteGame
checkComplete ((Frame (Spare _ _) _ _) : _) = Left IncompleteGame
checkComplete ((Frame (Filler2' _) _ _) : _) = Left IncompleteGame
checkComplete fs
  | n < 10 = Left IncompleteGame
  | otherwise = Right fs
  where
    n = length fs

scoreFold :: [FrameKind] -> ((Int, Int), Int) -- (# multipliers, score)
scoreFold = foldr go ((1, 1), 0)
  where
    go Strike ((m1, m2), score) = ((m2 + 1, 2), score + m1 * 10)
    go (Spare r r') ((m1, m2), score) = ((2, 1), score + m1 * r + m2 * r')
    go (Open r r') ((m1, m2), score) = ((1, 1), score + m1 * r + m2 * r')
    go (Filler1 r) ((m1, _), score) = ((1, 1), score + (m1 - 1) * r)
    go (Filler2 r r') ((m1, m2), score) = ((1, 1), score + (m1 - 1) * r + (m2 - 1) * r')
    go _ _ = error "Bug buggitty bug"
