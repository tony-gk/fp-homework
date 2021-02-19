module Block3.Task1
  ( maybeConcat
  , eitherConcat
  ) where

import Data.Maybe (fromJust)

-- | Return concatenation of all the 'Just' lists.
maybeConcat :: [Maybe [b]] -> [b]
maybeConcat = fromJust . foldr (<>) (Just [])

-- | Return a pair of results of a 'mappend' separately 
-- of elements inside 'Left' and elements inside 'Right'.
eitherConcat :: (Monoid a1, Monoid a2) => [Either a1 a2] -> (a1, a2)
eitherConcat eithers = foldr f (mempty, mempty) eithers
  where
    f (Left x) (left, right)  = (x <> left, right)
    f (Right x) (left, right) = (left, x <> right)
