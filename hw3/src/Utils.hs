module Utils
  ( resolvePath
  ) where

import System.FilePath (isAbsolute, joinPath, normalise, splitDirectories)

-- | 'resolvePath' @path1 path2@ resolved @path2@ relative to @path1@.
-- Folds paths with @..@ and removes @.@ entries.
-- If the result path indicates directory above the root, returns 'Nothing'.
resolvePath :: FilePath -> FilePath -> Maybe FilePath
resolvePath path1 path2 =
  let
    splitted1 = splitDirectories path1
    splitted2 = splitDirectories path2
    splitted = if isAbsolute path2 then splitted2 else splitted1 ++ splitted2
    canonicalize [] _ = Nothing
    canonicalize [lst] toSkip =
      if toSkip > 0
        then Nothing
        else Just [lst]
    canonicalize (next : rest) toSkip =
      case next of
        "."  -> canonicalize rest toSkip
        ".." -> canonicalize rest (toSkip + 1)
        _    -> if toSkip > 0
          then canonicalize rest (toSkip - 1)
          else (next :) <$> canonicalize rest toSkip
    in (normalise . joinPath . reverse) <$> canonicalize (reverse splitted) (0 :: Int)
