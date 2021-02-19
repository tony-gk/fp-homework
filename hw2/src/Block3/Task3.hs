module Block3.Task3
  ( correctBracketSequence
  , number
  ) where

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok, satisfy)

import Control.Applicative (Alternative ((<|>)), some)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (Foldable (foldl'))

-- | Parses correct bracket sequence.
correctBracketSequence :: Parser Char ()
correctBracketSequence = cbs *> eof
  where
    cbs = element '(' *> cbs *> element ')' *> cbs
        <|> ok

-- | Parses a digit. Returns the parsed character.
digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

-- | Parses signed integer. Returns the parsed 'Int'. 
number :: Parser Char Int
number = sign <*> (digitsToInt <$> digits)
  where
    sign = (0 -) <$ element '-'
       <|> id <$ element '+'
       <|> id <$ ok
    digits = some digit
    digitsToInt = foldl' (\acc next -> acc * 10 + next) 0
