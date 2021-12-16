module Diamond (diamond) where


import qualified Data.Char as C


diamond :: Char -> Maybe [String]
diamond c
  | not (C.isAsciiUpper c) = Nothing
  | otherwise = Just (_diamond c)

_diamond :: Char -> [String]
_diamond letter = [lineOf c | c <- ['A'..pred letter] ++ [letter,pred letter..'A']]
  where
    lineOf c = concat [wing c, _diamondLine c, wing c]
    wing c = replicate (offset letter - offset c) ' '
    _diamondLine 'A' = "A"
    _diamondLine c = let mid_space = 2 * offset c - 1 in
                       concat [[c], replicate mid_space ' ', [c]]
    offset c = C.ord c - C.ord 'A'
