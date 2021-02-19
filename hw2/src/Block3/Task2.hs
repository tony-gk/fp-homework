module Block3.Task2
  ( ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import Block3.Task1 (Parser (..))

import Control.Monad (guard)

-- | The parser @ok@ never fails and doesn't consume the input.
ok :: Parser s ()
ok = return ()

-- | This parser only succeeds at the end of the input.
eof :: Parser s ()
eof = Parser $ \input ->
  case input of
    [] -> Just ((), [])
    _  -> Nothing

-- | The parser @satisfy f@ succeeds for any character for which the supplied
-- function @p@ returns True. Returns the character that is actually parsed.
satisfy :: Eq a => (a -> Bool) -> Parser a a
satisfy predicate = Parser $ \input -> do
  guard (input /= [])
  let (x : rest) = input
  guard (predicate x)
  return (x, rest)

-- | @elemnt el@ parses a single element @el@. 
-- Returns the parsed element (i.e. @el@).
element :: Eq a => a -> Parser a a
element el = satisfy (== el)

-- | @stream s@ parses a list of elements given by s.
-- Returns the parsed list (i.e. @s@).
stream :: Eq a => [a] -> Parser a [a]
stream s = traverse element s
