{-# LANGUAGE InstanceSigs #-}
module Block1.Task2
  ( Tree(..)
  ) where

import Control.Applicative (Applicative (liftA2))

-- | Data type representing binary tree.
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf
  Leaf f <*> Leaf x     = Leaf (f x)
  Leaf f <*> Branch l r = Branch (Leaf f <*> l) (Leaf f <*> r)
  Branch lf rf <*> t    = Branch (lf <*> t) (rf <*> t)

instance Foldable Tree where
  foldMap f (Leaf x)     = mempty <> f x
  foldMap f (Branch l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  sequenceA (Leaf x)     = fmap Leaf x
  sequenceA (Branch l r) = liftA2 Branch (sequenceA l) (sequenceA r)
