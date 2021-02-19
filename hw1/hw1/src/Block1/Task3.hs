module Block1.Task3
  ( Tree(..)
  , isEmpty
  , size
  , find
  , insert
  , remove
  , fromList
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

-- | Data type representing binary tree.
data Tree a
  -- | Empty tree
  = Nil
  -- | Node with two children and data
  | Node
    (Tree a)      -- ^ Left child
    (NonEmpty a)  -- ^ Node data
    (Tree a)      -- ^ Right child
  deriving Show

instance Eq a => Eq (Tree a) where
  Nil == Nil = True
  (Node l1 xs r1) == (Node l2 ys r2) =
    l1 == l2 && xs == ys && r1 == r2
  _   == _   = False

-- | Test whether the tree is empty.
isEmpty :: Tree a -> Bool
isEmpty Nil = True
isEmpty _   = False

-- | Return total number of elements in nodes.
size :: Tree a -> Int
size Nil           = 0
size (Node l xs r) = size l + NE.length xs + size r

-- | 'find' @x@ @tree@ returns 'NonEmpty' list with elements equal @x@,
-- or 'Nothing' if there is no such node.
find :: Ord t => t -> Tree t -> Maybe (NonEmpty t)
find _ Nil = Nothing
find x (Node l ys r)
  | y == x    = Just ys
  | x < y     = find x l
  | otherwise = find x r
  where y = NE.head ys

-- | If @tree@ already has a node with a 'NonEmpty' list of elements @x@,
-- 'insert' @x@ @tree@ will insert @x@ in the list. Otherwise, it will
-- create a new node with 1-size list.
insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Node Nil (NE.fromList [x]) Nil
insert x (Node l ys r)
  | x == y    = Node l (NE.cons x ys) r
  | x < y     = Node (insert x l) ys r
  | otherwise = Node l ys (insert x r)
  where
    y = NE.head ys

-- | Construct a tree from a list.
fromList :: Ord a => [a] -> Tree a
fromList list = go list Nil
  where
    go (x : xs) t = go xs (insert x t)
    go [] t       = t

-- | 'remove' removes element from a tree. If the last element
-- remains in the 'NonEmpty' list in the node, removes the node.
remove :: Ord a => a -> Tree a -> Tree a
remove _ Nil = Nil
remove x (Node l ys@(y :| yrest) r)
  | x < y       = Node (remove x l) ys r
  | x > y       = Node l ys (remove x r)
  | yrest /= [] = Node l (NE.fromList yrest) r
  | otherwise   = union l r

-- | 'union' @left@ @right@ unions @left@ and @right@ trees.
-- Any of element of the @left@ tree must be less than any
-- elemnt of the @right@ tree.
union :: Tree a -> Tree a -> Tree a
union Nil Nil = Nil
union Nil r   = r
union l   Nil = l
union (Node l1 xs r1) (Node l2 ys r2) =
  Node (Node l1 xs (union r1 l2)) ys r2
