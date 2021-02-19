module Block2.Task1Spec
  ( testTree
  ) where

import Block1.Task3 (fromList)
import Block2.Task1 ()

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Foldable (Foldable (toList))
import Data.List (sort)

testTree :: IO TestTree
testTree = do
  let propTree = testProperty "fromList . toList == sort" prop_FromList_ToList
  return $ testGroup "Task1" [propTree]

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 10000)

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 100) genInt

prop_FromList_ToList :: Property
prop_FromList_ToList = property $ do
  xs <- forAll genIntList
  toList (fromList xs) === sort xs
