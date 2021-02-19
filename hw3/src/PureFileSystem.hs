{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module PureFileSystem
  ( FilePath
  , FileContent
  , FSElem(..)
  , name
  , permissions
  , modificationTime
  , dirContents
  , fileContents
  , FSState
  , FSException(..)
  , PureFS
  , runPFS
  , execPFS
  , evalPFS
  , defaultPermissions
  ) where

import qualified Data.Text as T

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (MonadState, State, runState)
import Data.Monoid (Endo (Endo), appEndo)
import Data.Time (UTCTime, addUTCTime)
import Lens.Micro (Lens', Traversal', each, filtered, has, lens, (^.), (^..), _1, _2, _3)
import Lens.Micro.Mtl (preuse, use, (%=), (.=))
import System.Directory (Permissions, emptyPermissions, readable, setOwnerReadable,
                         setOwnerSearchable, setOwnerWritable, writable)

import Control.Monad (void, when)
import FileSystemInterface (CommonInfo (..), FSActions (..), Info (..), ModificationTime)
import System.FilePath (splitDirectories, splitFileName, takeExtension, (</>))
import Utils (resolvePath)

type FileContent = T.Text

-- | Representation of the file system element.
data FSElem
  = Dir                                        -- Represention of directory
  { _name             :: FilePath              
  , _dirContents      :: [FSElem]             
  , _permissions      :: Permissions         
  , _modificationTime :: ModificationTime
  }
  | File                                       -- Represention of file
  { _name             :: FilePath
  , _fileContents     :: FileContent
  , _permissions      :: Permissions
  , _modificationTime :: ModificationTime
  }
  deriving (Show, Eq)

-- | '_File' targets the value contained in a 'FSElem', provided it's a 'File'.
_File :: Traversal' FSElem FSElem
_File a file@(File {}) = a file
_File _ dir            = pure dir

-- | '_Dir' targets the value contained in a 'FSElem', provided it's a 'Dir'.
_Dir :: Traversal' FSElem FSElem
_Dir a dir@(Dir {}) = a dir
_Dir _ file         = pure file

-- | Lens for name field of 'FSElem'.
name :: Lens' FSElem FilePath
name = lens _name (\obj field -> obj { _name = field})

-- | Lens for permissions field of 'FSElem'.
permissions :: Lens' FSElem Permissions
permissions = lens _permissions
  (\obj field -> obj { _permissions = field })

-- | Lens for modification time field of 'FSElem'.
modificationTime :: Lens' FSElem ModificationTime
modificationTime = lens _modificationTime
  (\obj field -> obj { _modificationTime = field })

-- | Targets directory contents of 'FSElem', provided it's a 'Dir'.
dirContents :: Traversal' FSElem [FSElem]
dirContents = _Dir . contents
  where
    contents = lens _dirContents
        (\obj field -> obj { _dirContents = field })

-- | Targets file content of 'FSElem', provided it's a 'File'.
fileContents :: Traversal' FSElem FileContent
fileContents = _File . contents
  where
    contents = lens _fileContents
        (\obj field -> obj { _fileContents = field })

-- | Targets 'FSElem' of a directory content with the given name.
subElementT :: FilePath -> Traversal' FSElem FSElem
subElementT elName = dirContents.each.filtered withName
  where
    withName el = el ^. name == elName

-- | Targets subdirectory with the given name of a 'Dir'.
subDirT :: FilePath -> Traversal' FSElem FSElem
subDirT dirName = subElementT dirName . _Dir

-- | Targets subfile with the given name of a 'Dir'.
subFileT :: FilePath -> Traversal' FSElem FSElem
subFileT fileName = subElementT fileName . _File

-- | Targets subdirectory of a 'Dir' recursively by the given path.
subDirRecurT :: FilePath -> Traversal' FSElem FSElem
subDirRecurT path = appEndo $ foldMap (Endo . subDirT) (splitDirectories path)


-- | Data type represents all types of filesystem errors.
data FSException
  = PathCannotBeResolvedException
  | DirectoryNotFoundException
  | DirectoryAlreadyExistsException
  | FileNotFoundException
  | FileAlreadyExistsException
  | FSElementNotFoundException
  | PermissionDeniedException
  deriving (Show, Eq)

-- | Data type of the current state of the filesystem. 
type FSState = (FilePath, UTCTime, FSElem)

-- | Date type represents computation in the filesystem.
newtype PureFS a = PureFS { getPFS :: ExceptT FSException (State FSState) a}
  deriving (Functor, Applicative, Monad, MonadState FSState, MonadError FSException)

-- | Adds dummy root to the inital state for simplified
-- work with the filesystem.
addDummyRoot :: FSState -> FSState
addDummyRoot (p, t, r) = (p, t, dummy)
  where
    dummy = Dir "dummy" [r] defaultPermissions (r ^. modificationTime)

-- | Unwraps 'PureFS' monad computation as a function.
runPFS :: PureFS a -> FSState -> (Either FSException a, FSState)
runPFS pfs initState =
  runState (runExceptT $ getPFS pfs) (addDummyRoot initState)

-- | Evaluates filesystem computation with the given initial sate
-- and returns the result, discarding the final state.
evalPFS :: PureFS a -> FSState -> Either FSException a
evalPFS pfs initState = fst $ runPFS pfs initState

-- | Evaluates filesystem computation with the given initial sate
-- and returns the final state, discarding the result.
execPFS :: PureFS a -> FSState -> FSState
execPFS pfs initState = snd $ runPFS pfs initState

-- | Lens for the current directory in 'FSState'
curDir :: Lens' FSState FilePath
curDir = _1

-- | Lens for the current time in 'FSState'
curTime :: Lens' FSState UTCTime
curTime = _2

-- | Lens for the filesystem in 'FSState'
root :: Lens' FSState FSElem
root = _3

fsAction :: PureFS a -> PureFS a
fsAction action = do
  curTime %= addUTCTime 1
  action

withResolvedPath :: FilePath -> (FilePath -> PureFS a) -> PureFS a
withResolvedPath path action = fsAction $ do
  curDirPath <- use curDir
  case resolvePath curDirPath path of
    Nothing       -> throwError PathCannotBeResolvedException
    Just resolved -> action resolved

instance FSActions PureFS where
  cd path = withResolvedPath path $ \resolved -> do
    void $ findDir resolved
    curDir .= resolved

  ls path = withResolvedPath path $ \resolved -> do
    dir <- findDir resolved
    checkReadable dir
    return (dir ^.. dirContents.each.name)

  cat path = withResolvedPath path $ \resolved -> do
    file <- findFile resolved
    checkReadable file
    return $ file ^. fileContents

  mkdir path = withResolvedPath path $ \resolved -> do
    let (dirPath, dirName) = splitFileName resolved
    baseDir <- findDir dirPath
    checkPermissions baseDir
    withDir resolved
      (const $ throwError DirectoryAlreadyExistsException)
      (return ())
    time <- use curTime
    let newDir = Dir dirName [] defaultPermissions time
        baseDirT = subDirRecurT dirPath
    root.baseDirT.dirContents %= (newDir :)
    root.baseDirT.modificationTime .= time

  mkfile path = withResolvedPath path $ \resolved -> do
    let (dirPath, fileName) = splitFileName resolved
    baseDir <- findDir dirPath
    checkPermissions baseDir
    withFile resolved
      (const $ throwError FileAlreadyExistsException)
      (return ())
    time <- use curTime
    let newFile = File fileName T.empty defaultPermissions time
        baseDirT = subDirRecurT dirPath
    root.baseDirT.dirContents %= (newFile :)
    root.baseDirT.modificationTime .= time

  remove path = withResolvedPath path $ \resolved -> do
    let (dirPath, baseName) = splitFileName resolved
        baseDirT = subDirRecurT dirPath
    fsElem <- findFSElem resolved
    checkPermissions fsElem
    root.baseDirT.dirContents %= filter (\el -> el ^. name /= baseName)

  write path text = withResolvedPath path $ \resolved -> do
    let (dirPath, fileName) = splitFileName resolved
    file <- findFile resolved
    checkWritable file
    let fileT = root.subDirRecurT dirPath.subFileT fileName
    time <- use curTime
    fileT.fileContents .= text
    fileT.modificationTime .= time

  info path = withResolvedPath path $ \resolved -> do
    fsElem <- findFSElem resolved
    return $ getInfo fsElem resolved

  find elName = do
    startDir <- use root
    return $ findRecursively elName startDir

checkReadable :: FSElem -> PureFS ()
checkReadable el = do
  let r = readable $ el ^. permissions
  when (not r) $ throwError PermissionDeniedException

checkWritable :: FSElem -> PureFS ()
checkWritable el = do
  let w = writable $ el ^. permissions
  when (not w) $ throwError PermissionDeniedException

checkPermissions :: FSElem -> PureFS ()
checkPermissions el = checkReadable el >> checkWritable el

findFile :: FilePath -> PureFS FSElem
findFile path = withFile path return (throwError FileNotFoundException)

findDir :: FilePath -> PureFS FSElem
findDir path = withDir path return (throwError DirectoryNotFoundException)

withDir :: FilePath -> (FSElem -> PureFS a) -> PureFS a -> PureFS a
withDir path onExist onElse = do
  maybeDir <- preuse (root.subDirRecurT path)
  case maybeDir of
    Nothing  -> onElse
    Just dir -> onExist dir

withFile :: FilePath -> (FSElem -> PureFS a) -> PureFS a -> PureFS a
withFile path onExist onElse = do
  let (dirPath, fileName) = splitFileName path
  maybeFile <- preuse (root.subDirRecurT dirPath.subFileT fileName)
  case maybeFile of
    Nothing   -> onElse
    Just file -> onExist file

findFSElem :: FilePath -> PureFS FSElem
findFSElem path = do
  let (dirPath, baseName) = splitFileName path
      dirPathT = subDirRecurT dirPath
  maybeFsElem <- preuse $ root.dirPathT.subElementT baseName
  case maybeFsElem of
    Nothing     -> throwError FSElementNotFoundException
    Just fsElem -> return fsElem

defaultPermissions :: Permissions
defaultPermissions =
  setOwnerReadable True $
  setOwnerWritable True $
  setOwnerSearchable True $
  emptyPermissions

getInfo :: FSElem -> FilePath -> Info
getInfo el@(File {}) path =
  FileInfo (getCommonInfo el path) (takeExtension $ el ^. name)

getInfo el@(Dir {}) path =
  DirInfo (getCommonInfo el path) (length $ el ^. dirContents)

getCommonInfo :: FSElem -> FilePath -> CommonInfo
getCommonInfo fsElem path=
  CommonInfo
    path
    (fsElem ^. modificationTime)
    (fsElem ^. permissions)
    (getSize fsElem)

getSize :: FSElem -> Integer
getSize f@(File {}) = fromIntegral $ T.length $ f ^. fileContents
getSize d@(Dir {})  = sum $ map getSize $ d ^. dirContents

findRecursively :: String -> FSElem -> [FilePath]
findRecursively elName dir = doFind dir "./"
  where
    doFind d curPath =
      let
        withName el = el ^. name == elName
        good = filter withName (d ^. dirContents)
        subDirs = filter (has _Dir) (d ^. dirContents )
        subFound = map (\subd -> doFind subd (curPath </> (subd ^. name))) subDirs
        foundFile =
          if length good > 0
            then [curPath </> elName]
            else []
      in foundFile ++ concat subFound
