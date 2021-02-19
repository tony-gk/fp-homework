{-# LANGUAGE LambdaCase #-}
module CommandLine 
    ( FSCommand(..)
    , parse
    )where

import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)

import Options.Applicative

-- | Representation of user input command.
data FSCommand =
  Cd FilePath                  -- Change directory
  | Dir                        -- List the contents of the current directory
  | Ls FilePath                -- List the contents of the specified directory
  | Cat FilePath               -- Get file content
  | MkDirectory String         -- Create new directory
  | MkFile String              -- Create new file
  | Remove FilePath            -- Remove file or directory
  | WriteFile FilePath         -- Write to file
  | FindFile String            -- Find file
  | Information FilePath       -- Get information about file or directory
  | Exit                       -- Exit
  deriving (Show, Eq)
  
-- | Tries to parse command entered by user.
-- On failure prints help page.
parse :: String -> IO (Maybe FSCommand)
parse s = handleResult $ execParserPure parserPrefs parserInfo (words s)
  where 
    parserPrefs = prefs showHelpOnError
    parserInfo = info (actionParser <**> helper) idm

handleResult :: ParserResult FSCommand -> IO (Maybe FSCommand)
handleResult (Success a) = return $ Just a

handleResult (Failure failure) = do
      let (msg, _) = renderFailure failure ""
      hPutStrLn stderr msg
      return Nothing

handleResult (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      return Nothing

actionParser :: Parser FSCommand
actionParser = subparser
   ( buildCommand "dir" Dir 
      "List content of the current directory"
  <> buildCommand "exit" Exit 
      "Exit"
  <> buildArgCommand "cd" Cd "PATH" "directory path" 
      "Change directory"
  <> buildArgCommand "ls" Ls "PATH" "directory path" 
      "List content of the specified directory"
  <> buildArgCommand "cat" Cat "PATH" "file path"
      "Print file content"
  <> buildArgCommand "mkdir" MkDirectory "NAME" "directory name" 
      "Create directory"
  <> buildArgCommand "mkfile" MkFile "NAME" "file name" 
      "Create file"
  <> buildArgCommand "remove" Remove "PATH" "path to file or directory" 
      "Remove file or directory"
  <> buildArgCommand "write-file" WriteFile "PATH" "path to file" 
      "Write text to file"
  <> buildArgCommand "find-file" FindFile "NAME" "name of file" 
      "Search for file in a directory hierarchy"
  <> buildArgCommand "info" Information "PATH" "path to file or directory"
      "Print information about file or directory"
  )

buildCommand :: String -> FSCommand -> String -> Mod CommandFields FSCommand
buildCommand name fscommand desc = 
  command name (info (pure fscommand) (progDesc desc))

buildArgCommand :: String -> (String -> FSCommand) -> String -> String -> String -> Mod CommandFields FSCommand
buildArgCommand name fscommand arg argHelp desc =
  command name (info (fscommand <$> (strArgument $ metavar arg <> help argHelp)) (progDesc desc))
