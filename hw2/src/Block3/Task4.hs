module Block3.Task4
  ( listlistParser
  ) where

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, satisfy)
import Block3.Task3 (number)

import Control.Applicative (Alternative (many), (<|>))
import Control.Monad (when)
import Data.Char (isSpace)

skipSpaces :: Parser Char ()
skipSpaces = () <$ many (satisfy isSpace)

-- | @parseInts n@ parses @n@ integers delimeted by comma.
parseInts :: Int -> Parser Char [Int]
parseInts 0 = return []

parseInts cnt = do
  skipSpaces
  _ <- element ','
  skipSpaces
  num <- number

  rest <- parseInts (cnt - 1)
  return (num : rest)

-- | Parses the size of the list and the corresponding number
-- of elements following it
listParser :: Parser Char [Int]
listParser = do
  skipSpaces
  size <- number
  when (size < 0) $ fail ""
  parseInts size

-- | Parses a list of lists of numbers delimeted by a comma.
-- Each list starts with the number of elements in it,
-- and then the elements of this list follow. For example:
--
-- > "2, 1,+10  , 3,5,-7, 2" == [ [1, 10], [5, -7, 2] ]
listlistParser :: Parser Char [[Int]]
listlistParser = listlist <* skipSpaces <* eof
  where
    listlist = (:) <$> listParser  <*> many (skipSpaces *> element ',' *> listParser)
              <|> return []
