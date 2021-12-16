module Hamming (distance) where

-- two working solutions

-- a shorter one

distance' :: String -> String -> Maybe Int
distance' xs ys
  | length xs == length ys = Just $ countDiff xs ys
  | otherwise              = Nothing
  where
    countDiff = ((length . filter not) .) . zipWith (==)
    --countDiff = (foldr (\x acc -> if x then acc else acc + 1) 0) $ (zipWith (==)) xs ys


-- and an longer, uglier one (but was more interesting to write)
distance :: String -> String -> Maybe Int
distance xs ys = sum <$> sequenceA (zipWithLongest (curry (if' 0 1 . uncurry (==))) xs ys)

zipWithLongest :: (a -> b -> c) -> [a] -> [b] -> [Maybe c]
zipWithLongest _ [] [] = []
zipWithLongest _ xs [] = map (const Nothing) xs
zipWithLongest _ [] ys = map (const Nothing) ys
zipWithLongest f (x:xs) (y:ys) = Just (f x y) : zipWithLongest f xs ys

-- instead of importing Control.Conditional from cond package
if' :: a -> a -> Bool -> a
if' x _ True = x
if' _ y False = y
