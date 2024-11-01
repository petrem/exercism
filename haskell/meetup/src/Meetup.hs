module Meetup
  ( Weekday (..),
    Schedule (..),
    meetupDay,
  )
where

import Data.Time.Calendar
  ( Day,
    MonthOfYear,
    Year,
    firstDayOfWeekOnAfter,
    fromGregorian,
    isLeapYear,
  )
import Data.Time.Calendar.MonthDay (monthLength)

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Enum, Show)

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth
  deriving (Show)

meetupDay :: Schedule -> Weekday -> Year -> MonthOfYear -> Day
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
      Last -> subtract 6 . monthLength (isLeapYear year) $ month
