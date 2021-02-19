module FileSystemInterface
  ( CommonInfo(..)
  , Info(..)
  , FSActions(..)
  , ModificationTime
  , Size
  , Extension
  , FileCount
  ) where

import qualified Data.Text as T

import Data.Time (UTCTime)
import System.Directory (Permissions)

-- | Time of the last modification of a file or directory.
type ModificationTime = UTCTime

-- | Size of a file or directory
type Size = Integer

-- | Extension of a file.
type Extension = String

-- | Number of files and directories of the directory.
type FileCount = Int

-- | Common infromation about file and directory.
data CommonInfo = CommonInfo FilePath ModificationTime Permissions Size
  deriving (Show, Eq)

-- | File or directory information.
data Info = FileInfo CommonInfo Extension | DirInfo CommonInfo FileCount
  deriving (Show, Eq)

class Monad m => FSActions m where
  cd :: FilePath -> m ()
  ls :: FilePath -> m [String]
  cat :: FilePath -> m T.Text
  mkdir :: FilePath -> m ()
  mkfile :: FilePath -> m ()
  remove :: FilePath -> m ()
  write :: FilePath -> T.Text -> m ()
  info :: FilePath -> m Info
  find :: String -> m [FilePath]
