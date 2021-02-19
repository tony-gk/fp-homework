module Block1.Task2
  ( Nat(..)
  , sum
  , mult
  , subtract
  , fromInt
  , toInt
  , isEven
  , div
  , mod
  ) where

import Prelude hiding (div, mod, subtract, sum)

-- | Data type representing an integer in Peano arithmetic.
data Nat
  = Z      -- ^ constructor representing 0 in Peano arithmetic
  | S Nat  -- ^ constructor representing successor of 'Nat'
  deriving Show

instance Eq Nat where
  Z   == Z   = True
  S x == S y = x == y
  _   == _   = False

instance Ord Nat where
  compare Z (S _)     = LT
  compare (S _) Z     = GT
  compare Z Z         = EQ
  compare (S x) (S y) = compare x y

-- | Return sum of natural numbers.
sum :: Nat -> Nat -> Nat
sum x (S y) = sum (S x) y
sum x Z     = x

-- | Return multiplication of natural numbers.
mult :: Nat -> Nat -> Nat
mult _ Z     = Z
mult x (S y) = sum (mult x y) x

-- | 'substract' @n@ @m@ returns subtraction @n@ from @m@.
-- If @n@ less than @m@ returns 'Z'.
subtract :: Nat -> Nat -> Nat
subtract (S x) (S y) = subtract x y
subtract x Z         = x
subtract Z _         = Z

-- | Convert 'Nat' to 'Int'.
-- /Beware/: passing a negative integer as the argument will cause an error.
fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n
  | n > 0     = S $ fromInt (n - 1)
  | otherwise = error "negative integer"

-- | Convert 'Nat' to 'Int'.
toInt :: Nat -> Int
toInt Z     = 0
toInt (S x) = 1 + toInt x

-- | Test whether natural number is even.
isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S x)) = isEven x

-- | Return division of natural numbers.
div :: Nat -> Nat -> Nat
div x y
  | x < y     = Z
  | y == Z    = error "divide by zero"
  | otherwise = S $ div (subtract x y) y

-- | Return remainder of division.
mod :: Nat -> Nat -> Nat
mod x y
  | x < y     = x
  | y == Z    = error "divide by zero"
  | otherwise = mod (subtract x y) y
