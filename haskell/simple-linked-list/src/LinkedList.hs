module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | List a (LinkedList a) deriving (Eq, Show)

instance Foldable LinkedList where
  foldr _ z Nil = z
  foldr f z (List x xs) = foldr f (f x z) xs

datum :: LinkedList a -> a
datum Nil = error "Empty LinkedList"
datum (List x _) = x


fromList :: [a] -> LinkedList a
fromList = foldr List Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new = List

next :: LinkedList a -> LinkedList a
next Nil = Nil
next (List _ listTail) = listTail

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldr List Nil

toList :: LinkedList a -> [a]
toList = foldl (flip (:)) []
