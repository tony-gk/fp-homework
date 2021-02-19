module Block3.Task1
  ( Parser(..)
  ) where

import Control.Applicative (Alternative (empty, (<|>)))

-- | Parser type with stream type @s@ and return type @a@
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f p = Parser $ \input -> fmap mapPair (runParser p input)
    where
      mapPair (a, rest) = (f a, rest)

instance Applicative (Parser s) where
  pure x = Parser $ \input -> pure (x, input)
  pf <*> px = Parser $ \input -> do
      (f, rest) <- runParser pf input
      (x, rest') <- runParser px rest
      return (f x, rest')

instance Monad (Parser s) where
  return = pure
  p >>= k = Parser $ \input -> do
    (x, rest) <- runParser p input
    runParser (k x) rest

instance Alternative (Parser s) where
  empty = Parser $ \_ -> Nothing
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

instance MonadFail (Parser s) where
  fail _ = empty