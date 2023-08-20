module Brackets
  ( arePaired
  ) where

import Control.Monad (foldM)
import Data.Maybe (mapMaybe)

-- Let's go monadic (aka banannas)
arePaired :: String -> Bool
arePaired = maybe False null . foldM go [] . mapMaybe toBracket
  where
    go stack (Opening shape) = Just (shape : stack)
    go [] (Closing _) = Nothing
    go (stackTop:stackTail) (Closing shape)
      | shape == stackTop = Just stackTail
      | otherwise = Nothing

data BracketShape
  = RoundBracket
  | SquareBracket
  | CurlyBracket
  deriving (Eq, Show)

data Bracket a
  = Opening a
  | Closing a
  deriving (Eq, Show)

toBracket :: Char -> Maybe (Bracket BracketShape)
toBracket '(' = Just (Opening RoundBracket)
toBracket ')' = Just (Closing RoundBracket)
toBracket '[' = Just (Opening SquareBracket)
toBracket ']' = Just (Closing SquareBracket)
toBracket '{' = Just (Opening CurlyBracket)
toBracket '}' = Just (Closing CurlyBracket)
toBracket _ = Nothing
{-
-- Improved a little bit more (I think): less utility functions, simpler looking logic
arePaired :: String -> Bool
arePaired = maybe False null . foldr go (Just []) . mapMaybe toBracket
  where
    go _ Nothing = Nothing
    go (Closing shape) (Just stack) = Just (shape : stack)
    go (Opening _) (Just []) = Nothing
    go (Opening shape) (Just (stackTop:stackTail))
      | shape == stackTop = Just stackTail
      | otherwise = Nothing
-}
{-
-- improved a bit with Maybe; can we do better?
-- e.g. this isn't short-cutting the fold, but could we?
arePaired''' :: String -> Bool
arePaired''' = maybe False null . foldr go (Just []) . filter isParens
  where
    go _ Nothing = Nothing
    go c (Just stack)
      | isClosingParens c = Just (c : stack)
      | otherwise =
        if not (null stack) && (c, head stack) `elem` pairedParens
          then Just (tail stack)
          else Nothing

pairedParens :: [(Char, Char)]
pairedParens = [('(', ')'), ('[', ']'), ('{', '}')]

isClosingParens :: Char -> Bool
isClosingParens c = c `elem` map snd pairedParens

isParens :: Char -> Bool
isParens c = c `elem` foldr (\(l, r) acc -> l : r : acc) [] pairedParens

-}
{-
-- foldr based solution that is even uglier.
arePaired'' :: String -> Bool
arePaired'' =
  ((&&) <$> snd <*> null . fst) . foldr go ([], True) . filter isParens
  where
    go _ (_, False) = (undefined, False)
    go c ([], True)
      | isClosingParens c = ([c], True)
      | otherwise = (undefined, False)
    go c (stack@(s:ss), True)
      | isClosingParens c = (c : stack, True)
      | otherwise = (ss, (c, s) `elem` pairedParens)
-}
{-
-- A recursive, fairly ugly solution.
arePaired' :: String -> Bool
arePaired' = arePairedHelper [] . filter isParens

arePairedHelper :: [Char] -> [Char] -> Bool
arePairedHelper stack [] = null stack
arePairedHelper [] (p:ps) = isOpeningParens p && arePairedHelper [p] ps
arePairedHelper stack@(s:ss) (p:ps) =
  if isOpeningParens p
    then arePairedHelper (p : stack) ps
    else (s, p) `elem` pairedParens && arePairedHelper ss ps

isOpeningParens :: Char -> Bool
isOpeningParens c = c `elem` map fst pairedParens
-}
