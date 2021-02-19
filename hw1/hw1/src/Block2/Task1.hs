{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Block2.Task1 where

import Block1.Task3 (Tree (..))

-- | 'Foldable' instance for a 'Tree'.
-- Folding occurs in the following order: right child, root, left child.
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ ini Nil = ini
  foldr f ini (Node l xs r) = foldr f foldedRAndList l
    where
      foldedR = foldr f ini r
      foldedRAndList = foldr f foldedR xs

  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap _ Nil           = mempty
  foldMap f (Node l xs r) = foldMap f l <> foldMap f xs <> foldMap f r
