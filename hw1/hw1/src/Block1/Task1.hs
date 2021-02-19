{-# LANGUAGE LambdaCase #-}

module Block1.Task1
  ( DayOfWeek(..)
  , nextDay
  , afterDay
  , isWeekend
  , daysToParty
  ) where

-- | Data type representing one of the days of the week.
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

instance Eq DayOfWeek where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

-- | Return next day of the week.
nextDay :: DayOfWeek -> DayOfWeek
nextDay = \case
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | 'afterDay' @n@ @day@ returns the 'DayOfWeek' following @day@ after @n@ days.
-- @n@ must be non-negative.
afterDay :: Int -> DayOfWeek -> DayOfWeek
afterDay n day = (iterate nextDay day) !! (n `mod` 7)

-- | Test whether the day is the weekend.
isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Return number of days until @Friday@
daysToParty :: Num p => DayOfWeek -> p
daysToParty Friday = 0
daysToParty day    = 1 + daysToParty (nextDay day)
