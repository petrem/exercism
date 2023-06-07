module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear = (isDivisibleBy 4) <&&> (isDivisibleBy 100 <->> isDivisibleBy 400)

isDivisibleBy :: Integral a => a -> a -> Bool
isDivisibleBy x y = (y `rem` x) == 0

-- | "Applicative" Implication
infixr 4 <->>
(<->>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 <->> p2 = (||) <$> not . p1 <*> p2

-- | "Applicative" Conjunction
infixr 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 <&&> p2 = (&&) <$> p1 <*> p2
