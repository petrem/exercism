module ReverseString
  ( reverseString
  ) where

import Data.Foldable (Foldable(foldl'))

reverseString :: (Foldable f, Monoid (f a), Applicative f) => f a -> f a
reverseString = foldl' (flip ((<>) . pure)) mempty
