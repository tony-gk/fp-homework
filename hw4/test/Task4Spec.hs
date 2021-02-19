module Task4Spec
  ( hprop_interpretIncrement
  , hprop_interpretLog2
  , hprop_interpretGcd
  ) where

import Task4

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInt :: Gen Int
genInt = Gen.int (Range.linear 1 10000)

hprop_interpretIncrement :: Property
hprop_interpretIncrement = property $ do
  n <- forAll genInt
  let res = interpret $ apply1 increment (sConst n)
  res === (n + 1)

hprop_interpretLog2 :: Property
hprop_interpretLog2 = property $ do
  n <- forAll genInt
  let res = interpret $ apply1 log2 (sConst n)
      expected = ceiling $ (logBase 2 (fromIntegral n) :: Double)
  res === expected
  
hprop_interpretGcd :: Property
hprop_interpretGcd = property $ do
  n <- forAll genInt
  m <- forAll genInt
  let res = interpret $ apply2 euclid (sConst n) (sConst m)
      expected = gcd n m
  res === expected