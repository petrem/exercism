module Brackets
  ( arePaired
  ) where

-- A recursive, fairly ugly solution.
arePaired :: String -> Bool
arePaired = arePairedHelper [] . filter isParens

arePairedHelper :: [Char] -> [Char] -> Bool
arePairedHelper stack [] = null stack
arePairedHelper [] (p:ps) = isOpeningParens p && arePairedHelper [p] ps
arePairedHelper stack@(s:ss) (p:ps) =
  if isOpeningParens p
    then arePairedHelper (p : stack) ps
    else (s, p) `elem` pairedParens && arePairedHelper ss ps

pairedParens :: [(Char, Char)]
pairedParens = [('(', ')'), ('[', ']'), ('{', '}')]

isOpeningParens :: Char -> Bool
isOpeningParens c = c `elem` map fst pairedParens

isParens :: Char -> Bool
isParens c = c `elem` foldr (\(l, r) acc -> l : r : acc) [] pairedParens
