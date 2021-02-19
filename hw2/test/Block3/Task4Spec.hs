module Block3.Task4Spec
  ( testTree
  ) where

import Block3.Task1
import Block3.Task4

import Data.List (intercalate)

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

testTree :: IO TestTree
testTree = do
  unit <- unitTestTree
  return $ testGroup "Parser listlistParser" [unit, propertyTestTree]

unitTestTree :: IO TestTree
unitTestTree = testSpec "Unit tests" $ do

  let
    test arg res = runParser listlistParser arg `shouldBe` res
    fails arg = test arg Nothing

  describe "Blank strings" $ do
    it "empty string" $ do
      test "" $ Just ([], "")
    it "spaces" $ do
      test "  " $ Just ([], "")
    it "whitespaces" $ do
      test "\t\n " $ Just ([], "")

  describe "Zero size" $ do
    it "one empty list" $ do
      test " 0 " $ Just ([[]], "")
    it "many empty lists" $ do
      test " 0, 0 ,  0,   0 " $ Just ([[],[],[],[]], "")

  describe "Fails on" $ do
    it "negative size of list" $ do
      fails " 0, -1 "
    it "not enough elements" $ do
      fails " 1, 0, 14, 1, 2, 3, 4"

buildListListString :: [[Int]] -> String
buildListListString xss = intercalate ", " $ map buildListString xss

buildListString :: [Int] -> String
buildListString xs =
  let len = length xs
  in if len == 0
    then "0"
    else (show len ++ ", ") ++ intercalate ", " (map show xs)

propertyTestTree :: TestTree
propertyTestTree = testProperty "Property based tests" $ property $ do
  let
    range = Range.linear 0 50 
    genInt = Gen.int (Range.linear minBound maxBound)

  listOfLists <- forAll $ Gen.list range (Gen.list range genInt)

  let
    listlist = buildListListString listOfLists

  runParser listlistParser listlist === Just (listOfLists, "")
