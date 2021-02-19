module Task4 where

import Control.Monad.Writer

gcdWithLog :: Integral a => a -> a -> (a, [(a, a)])
gcdWithLog a b = runWriter (gcdWriter a b)

gcdWriter :: (Integral a) => a -> a -> Writer [(a, a)] a
gcdWriter a b = do
  tell [(a, b)]
  if b == 0
    then return a
    else gcdWriter b (a `rem` b)