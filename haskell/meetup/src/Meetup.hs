module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar          (Day, fromGregorian, isLeapYear)
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
              deriving (Eq, Ord, Show, Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  let (_,_, startDay) = toWeekDate $ fromGregorian year month 1
      meetupWeekday = fromEnum weekday + 1
      startDayDiff = dayDistance startDay meetupWeekday

  in fromGregorian year month $
    case schedule of
      First  -> 1 + startDayDiff
      Second -> 8 + startDayDiff
      Third  -> 15 + startDayDiff
      Fourth -> 22 + startDayDiff
      Teenth ->
        let (_,_, thirteenthDay) = toWeekDate $ fromGregorian year month 13
          in 13 + dayDistance thirteenthDay meetupWeekday
      Last   -> let monthL = monthLength (isLeapYear year) month
                    (_,_,lastDay) = toWeekDate $ fromGregorian year month monthL
                in monthL - dayDistance meetupWeekday lastDay


dayDistance :: Int -> Int -> Int
dayDistance startDay endDay =
  if endDay >= startDay
     then endDay - startDay
     else (endDay - startDay) + 7
