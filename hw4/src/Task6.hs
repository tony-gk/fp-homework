{-# LANGUAGE RankNTypes #-}

module Task6
  ( FS(..)
  , name
  , contents
  , _File
  , _Dir
  , getContent
  , getDirName
  , getFileName
  , getFileNames
  , setRootName
  , addDirNameSuffix
  , firstSubDirName
  , getFS
  ) where

import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath ((</>), takeFileName)

import Lens.Micro (Lens', Traversal', each, ix, lens, (%~), (&), (.~), (^.), (^..), (^?))
import Control.Monad (filterM, liftM2)

data FS
  = Dir
  { _name     :: FilePath  
  , _contents :: [FS]
  }
  | File
  { _name     :: FilePath 
  }
  deriving Show

name :: Lens' FS FilePath
name = lens _name (\el newName -> el { _name = newName })

contents :: Lens' FS [FS]
contents = lens _contents (\el newConents -> el { _contents = newConents})

_File :: Traversal' FS FS
_File a file@(File _) = a file
_File _ dir           = pure dir

_Dir :: Traversal' FS FS
_Dir a dir@(Dir _ _) = a dir
_Dir _ file          = pure file

getContent :: FS -> [FS]
getContent fs = fs ^. _Dir . contents

getDirName :: FS -> Maybe FilePath
getDirName fs = fs ^? _Dir . name

getFileName :: FS -> FilePath
getFileName fs = fs ^. _File . name

setRootName :: FS -> FS
setRootName fs = fs & _Dir.name .~ "/"

addDirNameSuffix :: FS -> String -> FS
addDirNameSuffix fs suff = fs & _Dir.name %~ (++ suff)

firstSubDirName :: FS -> Maybe FilePath
firstSubDirName fs = fs ^. _Dir.contents ^.. each._File.name ^? ix 0

getFileNames :: FS -> [FilePath]
getFileNames fs = fs ^. _Dir.contents ^.. each._File.name

getFS :: FilePath -> IO FS
getFS path = do
  let pathName = takeFileName path
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, False) -> return (File pathName)
    (False, True) -> do
      let f = \p -> liftM2 (||) (doesFileExist p) (doesDirectoryExist p)
      dirContentPaths <- map (path </>) <$> listDirectory path
      filteredPaths <- filterM f dirContentPaths
      dirContents <- mapM getFS filteredPaths
      return $ Dir pathName dirContents 
    _ -> error $ "Path contains neither file nor directory: " ++ path
