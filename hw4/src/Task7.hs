{-# LANGUAGE RankNTypes #-}

module Task7 
  ( cd
  , ls
  , file
  ) where

import Task6 (FS (..), contents, name, _Dir, _File)

import Lens.Micro (Traversal', each, filtered, (^.))

cd :: FilePath -> Traversal' FS FS
cd dirName = _Dir.contents.each._Dir.filtered filt
  where
    filt :: FS -> Bool
    filt f = f ^. name == dirName

ls :: Traversal' FS FilePath
ls = _Dir.contents.each.name

file :: FilePath -> Traversal' FS FilePath
file fileName = _Dir.contents.each._File.name.filtered (== fileName)

