module Block1.Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

-- | Return sum of integers in the string, delimited by white space.
-- If at least one element of the string cannot be converted to an
-- integer, then Nothing is returned.
stringSum :: String -> Maybe Int
stringSum s = let
  readMaybeInt = readMaybe :: String -> Maybe Int
  maybeInts = map readMaybeInt $ words s
  in fmap sum $ sequenceA maybeInts
