module Block1.Task2Spec
  ( testTree
  ) where

import Prelude hiding (div, mod, subtract, sum)

import Block1.Task2

import Hedgehog (Gen, Property, PropertyT, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)


testTree :: IO TestTree
testTree = do
  unit <- unitTestTree
  return $ testGroup "Task2" [unit, propertyBasedTestTree]

-- | Unit tests
unitTestTree :: IO TestTree
unitTestTree = testSpec "Unit tests" $ do
  let nums = take 100 $ iterate S Z

  describe "fromInt" $ do
    it "0 from int" $ do
      fromInt 0 `shouldBe` Z
    it "15 from int" $ do
      fromInt 15 `shouldBe` nums !! 15
    it "42 from int" $ do
      fromInt 42 `shouldBe` nums !! 42

  describe "toInt" $ do
    it "(Nat 0) to int" $ do
      toInt Z `shouldBe` 0
    it "(Nat 9) to int" $ do
      toInt (nums !! 9) `shouldBe` 9
    it "(Nat 69) to int" $ do
      toInt (nums !! 69) `shouldBe` 69

-- | Property-based tests
propertyBasedTestTree :: TestTree
propertyBasedTestTree = testGroup "Property-based tests"
  [ testProperty "toInt . fromInt == id" prop_fromToInt
  , testProperty "sum" prop_sum
  , testProperty "multiplication" prop_mult
  , testProperty "subtraction" prop_sub
  , testProperty "equality instance" prop_eq
  , testProperty "ord instance" prop_less
  , testProperty "isEven" prop_isEven
  , testProperty "division" prop_div
  , testProperty "modulus" prop_mod
  ]

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 100)

prop_fromToInt :: Property
prop_fromToInt = property $ do
  n <- forAll genInt
  toInt (fromInt n) === n

twoIntsProperty :: (Int -> Int -> PropertyT IO ()) -> Property
twoIntsProperty expectations = property $ do
  n <- forAll genInt
  m <- forAll genInt
  expectations n m

prop_sum :: Property
prop_sum = twoIntsProperty $ \n m -> do
  toInt (sum (fromInt n) (fromInt m)) === n + m

prop_mult :: Property
prop_mult = twoIntsProperty $ \n m -> do
  toInt (mult (fromInt n) (fromInt m)) === n * m

prop_sub :: Property
prop_sub = twoIntsProperty $ \n m -> do
  toInt (subtract (fromInt n) (fromInt m)) === max 0 (n - m)

prop_eq :: Property
prop_eq = twoIntsProperty $ \n m -> do
  let n' = fromInt n
      m' = fromInt m
  assert $ n' == n'
  assert $ m' == m'
  (n' == m') === (n == m)

prop_less :: Property
prop_less = twoIntsProperty $ \n m -> do
  (fromInt n < fromInt m) === (n < m)

prop_isEven :: Property
prop_isEven = property $ do
  n <- forAll genInt
  isEven (fromInt n) === even n

prop_div :: Property
prop_div = property $ do
  n <- forAll genInt
  m <- forAll $ Gen.filter (> 0) genInt
  toInt (fromInt n `div` fromInt m) === n `quot` m

prop_mod :: Property
prop_mod = property $ do
  n <- forAll genInt
  m <- forAll $ Gen.filter (> 0) genInt
  toInt (fromInt n `mod` fromInt m) === n `rem` m
