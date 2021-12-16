{-# LANGUAGE TupleSections #-}
module Base (Error(..), rebase) where

import Data.Bool(bool)


data Error a
  = InvalidInputBase
  | InvalidOutputBase
  | InvalidDigit a
  deriving (Show, Eq)

rebase :: (Num a, Integral a) => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits = do
  _ <- validBase InvalidInputBase inputBase
  _ <- validBase InvalidOutputBase outputBase
  n <- fromBase inputBase inputDigits
  return $ toBase outputBase n

validBase :: Integral a => Error a -> a -> Either (Error a) [a]
validBase err = bool (Left err) (Right []) . (>= 2)

fromBase :: (Num a, Integral a) => a -> [a] -> Either (Error a) a
fromBase b =
  fmap fst . foldr (go b) (return (0, 1))
  where
    go ::(Num a, Integral a) => a -> a -> Either (Error a) (a, a) -> Either (Error a) (a, a)
    go _ _ err@(Left _)              = err
    go b d (Right (acc, multiplier)) = (, multiplier * b) . (acc +) . (multiplier *) <$> validDigit b d

validDigit :: Integral a => a -> a -> Either (Error a) a
validDigit b d
  | d < 0 || d >= b = Left (InvalidDigit d)
  | otherwise       = Right d

toBase :: Integral a => a -> a -> [a]
toBase _ 0 = []
toBase b n = let (q, r) = n `quotRem` b
             in toBase b q ++ [r]
