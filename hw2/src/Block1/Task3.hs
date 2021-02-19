module Block1.Task3
  ( NonEmpty(..)
  ) where

import Control.Applicative (Applicative(liftA2))

-- | Non-empty list type.
data NonEmpty a = a :| [a]

concatNE :: [NonEmpty a] -> [a]
concatNE [] = []
concatNE ((x :| xs) : rest) = x : xs ++ concatNE rest

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []
  (f :| fs) <*> (x :| xs)= first :| rest
    where
      first = f x
      rest = (f <$> xs) ++ (fs <*> x : xs)

instance Monad NonEmpty where
  return = pure
  (x :| xs) >>= k = first :| rest
    where
      (first :| firstResTail) = k x
      rest = firstResTail ++ (concatNE $ map k xs)

instance Foldable NonEmpty where
  foldMap f (x :| xs) = f x <> foldMap f xs
  
instance Traversable NonEmpty where
  sequenceA (x :| xs) = liftA2 (:|) x (sequenceA xs)