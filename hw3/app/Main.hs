module Main where

import CommandLine (FSCommand (..), parse)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Reader (MonadIO, liftIO)
import Data.IORef (newIORef)

import System.Directory (Permissions (..), getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hGetContents, hPutStrLn, isEOF, stdout)
import System.IO.Error (ioeGetErrorString, ioeGetFileName)

import FileSystem
import FileSystemInterface
import Utils (resolvePath)

main :: IO ()
main = do
  args <- getArgs
  curDir <- getCurrentDirectory
  curDirRef <- newIORef curDir
  case args of
    [] -> return ()
    [path] -> do
      withErrorHandling $ runFS (cd path) curDirRef
    _ -> do
      putStrLn "Expected optional argument: start directoty"
      exitFailure

  runFS mainLoop curDirRef

-- | Parses user input and executes commands until
-- exit command is occurred.
mainLoop :: FileSystem ()
mainLoop = do
  curDir <- getCurDir
  command <- liftIO $ do
    putStr $ curDir ++ " > "
    hFlush stdout
    input <- getLine
    command <- parse input
    return command

  case command of
    Nothing   -> mainLoop
    Just Exit -> return ()
    Just cmd  -> execCommand cmd >> mainLoop

-- | Executes specified command.
execCommand :: FSCommand -> FileSystem ()
execCommand cmd = withErrorHandling $ case cmd of
  Cd path          -> cd path
  Dir              -> listDir "."
  Ls path          -> listDir path
  Cat path         -> printContent path
  MkDirectory path -> mkdir path
  MkFile path      -> mkfile path
  Remove path      -> remove path
  WriteFile path   -> Main.writeFile path
  Information path -> printInfo path
  FindFile name    -> findFile name

-- | Runs given action with catching IO errors.
-- Handler prints information about error.
withErrorHandling :: (MonadCatch m, MonadIO m) => m () -> m ()
withErrorHandling action = catch action handler
  where
    handler ioe = liftIO $ putStrLn $ case ioeGetFileName ioe of
      Just filename -> filename ++ ": " ++ ioeGetErrorString ioe
      Nothing       -> ioeGetErrorString ioe

-- | Prints the contents of a directory.
listDir :: FilePath -> FileSystem ()
listDir path = do
  contents <- ls path
  liftIO $ mapM_ putStrLn contents

-- | Prints the contents of a file.
printContent :: FilePath -> FileSystem ()
printContent path = do
  content <- cat path
  liftIO $ T.putStrLn content

-- | Reads the input text up to the EOF and
-- writes to the file.
writeFile :: FilePath -> FileSystem ()
writeFile path = do
  liftIO $ putStrLn "Input the text:"
  input <- liftIO $ readTillEof
  write path input

-- | Reads lines up to the EOF symbol.
readLinesTillEof :: IO [T.Text]
readLinesTillEof = do
  done <- isEOF
  if done
    then return []
    else do
      line <- T.getLine
      rest <- readLinesTillEof
      return $ line : rest

-- | Reads text up to the EOF symbol.
readTillEof :: IO T.Text
readTillEof = readLinesTillEof >>= return . T.unlines

-- | Prints informatin about the specified file.
printInfo :: FilePath -> FileSystem ()
printInfo path = do
  inf <- info path
  liftIO $ T.putStrLn $ showInfo inf

-- | Prints absolute paths of files with specified name.
-- Search starts from the current directory in 'FileSystem'.
findFile :: String -> FileSystem ()
findFile name = do
  paths <- find name
  liftIO $ mapM_ putStrLn paths

-- | Converts 'Info' to pretty text form.
showInfo :: Info -> T.Text
showInfo (FileInfo common ext) =
  T.unlines $
  [ T.pack ("Extension: '" ++ ext ++ "'")
  , showCommonInfo common
  ]

showInfo (DirInfo common cnt) =
  T.unlines $
  [ T.pack ("File count: " ++ show cnt)
  , showCommonInfo common
  ]

-- | Converts 'CommonInfo' to pretty text form.
showCommonInfo :: CommonInfo -> T.Text
showCommonInfo (CommonInfo path time perms size) =
  T.unlines $ map T.pack [
    "Absolute path: " ++ path,
    "Modification time: " ++ show time,
    "Permissions: '" ++ showPermissions perms ++ "'",
    "Size: " ++ show size
  ]

-- | Converts 'Permissions' to Unix-like form.
showPermissions :: Permissions -> String
showPermissions perm = let
  r = if readable perm then "r" else ""
  w = if writable perm then "w" else ""
  e = if executable perm then "x" else ""
  s = if searchable perm then "s" else ""
  in r ++ w ++ e ++ s
