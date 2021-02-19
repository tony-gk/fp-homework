module Block2.Task2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty

-- | Return sublists separated by the given delimiter. For example:
-- > splitOn '/' "path/to/file" == "path" :| ["to", "file"]
splitOn :: (Eq a, Foldable t) => a -> t a -> NonEmpty [a]
splitOn delimiter lst =
  let f next (acc, splitted)
        | next == delimiter = ([], acc : splitted)
        | otherwise = (next : acc, splitted)
      (rest, parts) = foldr f ([], []) lst
   in fromList $ rest : parts

-- | Return the list joined with given delimiter. For example:
-- > joinWith '/' ("path" :| ["to", "file"]) == "path/to/file"
joinWith :: a -> NonEmpty [a] -> [a]
joinWith delimiter parts = Prelude.tail $ foldr f [] parts
  where
    f nextPart joined = delimiter : nextPart ++ joined
