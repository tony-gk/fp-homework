module Block2.Task2Spec
  ( testTree
  ) where

import Block2.Task2

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

import Data.List.NonEmpty (NonEmpty ((:|)))

testTree :: IO TestTree
testTree = do
  unit <- unitTestTree
  return $ testGroup "Task2" [unit, propertyBasedTestTree]

unitTestTree :: IO TestTree
unitTestTree = testSpec "Unit tests" $ do

  describe "splitOn" $ do
    it "empty list" $ do
      splitOn '-' "" `shouldBe` "" :| []
    it "delimiter at the beginning" $ do
      splitOn '-' "-abc-csd" `shouldBe` "" :| ["abc", "csd"]
    it "delimiter at the end" $ do
      splitOn '-' "ge-le-te-" `shouldBe` "ge" :| ["le", "te", ""]
    it "only delimters" $ do
      splitOn '-' "----" `shouldBe` "" :| ["", "", "", ""]

  describe "joinWith" $ do
    it "one empty part" $ do
      joinWith '.' ("" :| []) `shouldBe` ""
    it "first part is empty" $ do
      joinWith '.' ("" :| ["ab", "cd", "ef"]) `shouldBe` ".ab.cd.ef"
    it "last part is empty" $ do
      joinWith '.' ("k" :| ["s", "r", ""]) `shouldBe` "k.s.r."
    it "several empty parts" $ do
      joinWith '.' ("" :| ["", "", ""]) `shouldBe` "..."


propertyBasedTestTree :: TestTree
propertyBasedTestTree = testGroup "Property based test"
  [testProperty "joinWith x . splitOn x == id" prop_composition]

prop_composition :: Property
prop_composition = property $ do
  delim <- forAll $ Gen.lower
  let listWithMostlyDelims = ['a'..'z'] ++ replicate 10 delim
  str <- forAll $ Gen.list (Range.linear 0 100) (Gen.element listWithMostlyDelims)

  joinWith delim (splitOn delim str) === str
