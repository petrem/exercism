module Meetup (Weekday (..), Schedule (..), meetupDay) where

import Data.Time.Calendar (Day, firstDayOfWeekOnAfter, fromGregorian, gregorianMonthLength)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Enum

data Schedule = First | Second | Third | Fourth | Last | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  firstDayOfWeekOnAfter dayOfWeek (fromGregorian year month baseDayForSchedule)
  where
    dayOfWeek = toEnum . (1 +) . fromEnum $ weekday
    baseDayForSchedule = case schedule of
      First -> 1
      Second -> 8
      Third -> 15
      Fourth -> 22
      Teenth -> 13
      Last -> gregorianMonthLength year month - 6
