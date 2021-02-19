module PureFSSpec where

import qualified Data.Text as T
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime), secondsToDiffTime)
import System.Directory (emptyPermissions)

import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import FileSystemInterface
import PureFileSystem


nullTime :: UTCTime
nullTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

file :: FilePath -> FileContent -> FSElem
file fName contents = File fName contents defaultPermissions nullTime

emptyFile :: FilePath -> FSElem
emptyFile fName = file fName T.empty

dir :: FilePath -> [FSElem] -> FSElem
dir dName contents = Dir dName contents defaultPermissions nullTime

emptyDir :: FilePath -> FSElem
emptyDir dName = dir dName []

nonAccessDir :: FilePath -> FSElem
nonAccessDir dName = Dir dName [] emptyPermissions nullTime

nonAccessFile :: FilePath -> FSElem
nonAccessFile fName = File fName T.empty emptyPermissions nullTime

testDir :: FSElem
testDir = dir "/"
  [ emptyFile "f1"
  , dir "d1" [file "f11" (T.pack "wow"), dir "d11" []]
  , file "f2" $ T.pack "sometext"
  , dir "d2" []
  , emptyFile "f3"
  , nonAccessDir "nd"
  , nonAccessFile "nf"
  ]

testFSState :: FSElem -> FSState
testFSState fs = ("/", nullTime, fs)

evalCommand :: PureFS a -> Either FSException a
evalCommand command = evalPFS command (testFSState testDir)

execCommand :: PureFS a -> FSState
execCommand command = execPFS command (testFSState testDir)

getCurDir :: FSState -> FilePath
getCurDir (curDir, _, _) = curDir

shouldEval :: (Show a, Show b, Eq a, Eq b) => Either a b -> b -> Expectation
shouldEval res expect = res `shouldBe` Right expect

shouldThrow :: (Show a, Show b, Eq a, Eq b) => Either a b -> a -> Expectation
shouldThrow res expect = res `shouldBe` Left expect

spec :: Spec
spec = do
  describe "cd" $ do
    it "'.'" $ do
      getCurDir (execCommand $ cd ".") `shouldBe` "/"
    it "to subdir" $ do
      getCurDir (execCommand $ cd "d1") `shouldBe` "/d1"
      getCurDir (execCommand $ cd "d1/d11") `shouldBe` "/d1/d11"
    it "to non-existent" $ do
      evalCommand (cd "dddd") `shouldThrow` DirectoryNotFoundException

  describe "ls" $ do
    it "current directory" $ do
      evalCommand (ls ".")
        `shouldEval` ["f1", "d1", "f2", "d2", "f3", "nd", "nf"]
    it "subdirectory" $ do
      evalCommand (ls "d1") `shouldEval` ["f11", "d11"]
      evalCommand (cd "d2" >> ls ".") `shouldEval` []
    it "non-existent directory" $ do
      evalCommand (ls "d3")
        `shouldThrow` DirectoryNotFoundException
    it "non-readable directory" $ do
      evalCommand (ls "nd")
        `shouldThrow` PermissionDeniedException

  describe "cat" $ do
    it "just file" $ do
      evalCommand (cat "f1") `shouldEval` T.empty
      evalCommand (cat "f2") `shouldEval` (T.pack "sometext")
    it "non-existent file" $ do
      evalCommand (cat "fff")
        `shouldThrow` FileNotFoundException
    it "non-readable file" $ do
      evalCommand (cat "nf")
        `shouldThrow` PermissionDeniedException

  describe "mkdir" $ do
    it "just mkdir" $
      evalCommand (mkdir "d1/newDir" >> cd "d1/newDir")
        `shouldEval` ()
    it "already-existent dir" $ do
      evalCommand (mkdir "d1")
        `shouldThrow` DirectoryAlreadyExistsException
    it "in non-writable dir" $ do
      evalCommand (mkdir "nd/newDir")
        `shouldThrow` PermissionDeniedException

  describe "mkfile" $ do
    it "just mkdile" $ do
      evalCommand (mkfile "d1/newFile" >> cat "d1/newFile")
        `shouldEval` T.empty
    it "already-existent file" $ do
      evalCommand (mkfile "f1")
        `shouldThrow` FileAlreadyExistsException
    it "in non-writable dir" $ do
      evalCommand (mkfile "nd/newFile")
        `shouldThrow` PermissionDeniedException

  describe "remove" $ do
    it "file" $ do
      evalCommand (remove "d1/f11" >> ls "d1")
        `shouldEval` ["d11"]
    it "directory" $ do
      evalCommand (remove "d1/d11" >> ls "d1")
        `shouldEval` ["f11"]
    it "non-existent" $ do
      evalCommand (remove "d1/f22")
        `shouldThrow` FSElementNotFoundException

  describe "write" $ do
    it "empty text" $ do
      evalCommand (write "f2" T.empty >> cat "f2")
        `shouldEval` T.empty
    it "some text" $ do
      evalCommand (write "f3" (T.pack "text") >> cat "f3")
        `shouldEval` T.pack "text"
    it "to non-writable file" $ do
      evalCommand (write "nf" T.empty)
        `shouldThrow` PermissionDeniedException
    it "to non-existent file" $ do
      evalCommand (write "fkds" T.empty)
        `shouldThrow` FileNotFoundException
  
  describe "info" $ do 
    it "file" $ do
      evalCommand (info "f2") `shouldEval` 
        FileInfo (CommonInfo "/f2" nullTime defaultPermissions 8) ""
    it "directory" $ do
      evalCommand (info "d1") `shouldEval`
        DirInfo (CommonInfo "/d1" nullTime defaultPermissions 3) 2
    it "non-existent" $ do
      evalCommand (info "fdk")
        `shouldThrow` FSElementNotFoundException
  
  describe "find" $ do
    it "file" $ do
      evalCommand (find "f11") `shouldEval` ["/d1/f11"]
    it "directory" $ do
      evalCommand (find "d11") `shouldEval` ["/d1/d11"]
