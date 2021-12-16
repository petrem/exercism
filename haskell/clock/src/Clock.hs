module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)


data Clock = Time { getHour::Int
                  , getMinute::Int
                  } deriving (Eq, Ord)

instance Show Clock where
  show = printf "%0.2d:%0.2d" <$> getHour <*> getMinute

instance Semigroup Clock where
  Time hh1 mm1 <> Time hh2 mm2 = fromHourMin (hh1 + hh2) (mm1 + mm2)

-- not necessary for implementing the requirements, but makes sense to have
instance Monoid Clock where
  mempty = Time 0 0

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Time hh mm where
  hh = (hour + (minute `div` 60)) `mod` 24
  mm = minute `mod` 60

toString :: Clock -> String
toString = show

addDelta :: Int -> Int -> Clock -> Clock
addDelta hh mm = (Time hh mm <>)
