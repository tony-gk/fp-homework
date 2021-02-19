module Block3.Task2
  ( NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  ) where

-- | Non-empty list type.
data NonEmpty a = a :| [a]
  deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ [y] ++ ys)

-- | Data type representing at least one value.
data ThisOrThat a b = This a | That b | Both a b
  deriving (Show, Eq)

-- | The result of the semigroup operation is the 
-- leftmost 'This' or the rightmost 'That'.
instance Semigroup (ThisOrThat a b) where
  This x <> This _   = This x
  This x <> That y   = Both x y
  This x <> Both _ y = Both x y

  That y <> This x   = Both x y
  That _ <> That y   = That y
  That _ <> Both x y = Both x y

  Both x y <> This _   = Both x y
  Both x _ <> That y   = Both x y
  Both x _ <> Both _ y = Both x y

-- | Dot-concatenated names.
newtype Name = Name String
  deriving (Show, Eq)

instance Semigroup Name where
  Name "" <> Name y  = Name y
  Name x  <> Name "" = Name x
  Name x  <> Name y  = Name (x ++ "." ++ y)

instance Monoid Name where
  mempty = Name ""
  mappend = (<>)

-- | The monoid of endomorphisms under composition.
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo (\x -> x)
  mappend = (<>)
