{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FileSystem 
  ( FileSystem
  , runFS
  , getCurDir
  , setCurDir
  ) where

import qualified Data.Text.IO as T

import Control.Monad (filterM, when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), ask, runReaderT)
import Data.IORef (IORef, readIORef, writeIORef)

import System.Directory (createDirectory, doesDirectoryExist, doesPathExist, getFileSize,
                         getModificationTime, getPermissions, listDirectory, removePathForcibly)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (IOMode (ReadMode), withFile)
import System.IO.Error (ioeSetFileName)

import FileSystemInterface (CommonInfo (..), FSActions (..), Info (..))
import Utils (resolvePath)


-- | Date type represents computation in the real filesystem.
newtype FileSystem a = FS { getFS :: ReaderT (IORef FilePath) IO a }
  deriving (Functor, Applicative, Monad,
            MonadReader (IORef FilePath),
            MonadIO, MonadThrow, MonadCatch)

-- | Evaluates 'FileSystem' computation with 'IORef'
-- on the current directory in 'IO' monad.
runFS :: FileSystem a -> IORef FilePath -> IO a
runFS fs ref = runReaderT (getFS fs) ref

instance FSActions FileSystem where
  cd path = do
    curDir <- getCurDir
    resolved <- resolvePathIO curDir path
    dde <- liftIO $ doesDirectoryExist resolved
    if not dde
      then throwFSError "does not exist" resolved
      else setCurDir resolved

  ls path = withResolvedPath path $ \resolved -> do
    map takeFileName <$> listDirectory resolved

  cat path = withResolvedPath path $ \resolved -> do
    withFile resolved ReadMode T.hGetContents

  mkdir path = withResolvedPath path $ \resolved -> do
    createDirectory resolved

  mkfile path = withResolvedPath path $ \resolved -> do
    dpe <- doesPathExist resolved
    when dpe $ throwFSError "already exists" resolved
    writeFile resolved ""

  remove path = withResolvedPath path $ \resolved -> do
    removePathForcibly resolved

  write path text = withResolvedPath path $ \resolved -> do
    T.writeFile resolved text

  info path = withResolvedPath path $ \resolved -> do
    modifTime <- getModificationTime resolved
    permissions <- getPermissions resolved
    size <- getPathSize resolved
    let commonInfo = CommonInfo resolved modifTime permissions size
    isDir <- doesDirectoryExist resolved
    if isDir
      then do
        cnt <- length <$> listDirectory resolved
        return $ DirInfo commonInfo cnt
      else do
        let ext = takeExtension resolved
        return $ FileInfo commonInfo ext

  find name = do
    curDir <- getCurDir
    liftIO $ doFind curDir name

getCurDir :: FileSystem FilePath
getCurDir = do
  curDirRef <- ask
  liftIO $ readIORef curDirRef

setCurDir :: FilePath -> FileSystem ()
setCurDir dir = do
  curDirRef <- ask
  liftIO $ writeIORef curDirRef dir

throwFSError :: MonadIO m => String -> FilePath -> m a
throwFSError reason path =
  liftIO $ ioError $ userError reason `ioeSetFileName` path

resolvePathIO :: MonadIO m => FilePath -> FilePath -> m FilePath
resolvePathIO path1 path2 =
  case resolvePath path1 path2 of
    Just resolved -> return resolved
    Nothing       -> throwFSError "path cannot be resolved" path2

withResolvedPath :: FilePath -> (FilePath -> IO a) -> FileSystem a
withResolvedPath path action = do
  curDir <- getCurDir
  resolved <- resolvePathIO curDir path
  liftIO $ action resolved

getPathSize :: FilePath -> IO Integer
getPathSize path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      files <- listDirectory path
      let resolvedFiles = map (path </>) files
      sum <$> mapM getPathSize resolvedFiles
    else getFileSize path

doFind :: FilePath -> String -> IO [FilePath]
doFind curDir name = do
  contents <- listDirectory curDir
  let filesWithName = filter (== name) contents
      pathsWithName = map (curDir </>) filesWithName
  subDirs <- filterM doesDirectoryExist pathsWithName
  found <- concat <$> mapM (\d -> doFind d name) subDirs
  return $ pathsWithName ++ found
