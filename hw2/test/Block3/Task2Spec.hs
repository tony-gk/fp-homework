module Block3.Task2Spec
  ( testTree
  ) where

import Block3.Task1
import Block3.Task2

import Control.Monad.Identity

import Hedgehog (GenT, MonadGen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

testTree :: IO TestTree
testTree = return $ testGroup "Basic combinators"
  [ testProperty "ok" prop_ok
  , testProperty "eof" prop_eof
  , testProperty "satisfy" prop_satisfy
  , testProperty "element" prop_element
  , testProperty "stream" prop_stream
  ]

stringGen :: GenT Identity String
stringGen = stringGenRange 0 10

stringGenRange :: MonadGen m => Int -> Int -> m String
stringGenRange from to = Gen.string (Range.linear from to) Gen.ascii

prop_ok :: Property
prop_ok = property $ do
  s <- forAll stringGen
  runParser ok s === Just ((), s)

prop_eof :: Property
prop_eof = property $ do
  s <- forAll stringGen
  case s of
    [] -> runParser eof s === Just ((), s)
    _  -> runParser eof s === Nothing

prop_satisfy :: Property
prop_satisfy = property $ do
  s <- forAll $ stringGenRange 1 10
  b <- forAll $ Gen.bool
  if b
    then runParser (satisfy $ const b) s === Just (head s, tail s)
    else runParser (satisfy $ const b) s === Nothing

prop_element :: Property
prop_element = property $ do
  s <- forAll $ stringGenRange 1 10
  c <- forAll $ Gen.filter (/= head s) Gen.ascii

  runParser (element $ head s) s === Just (head s, tail s)
  runParser (element c) s === Nothing

prop_stream :: Property
prop_stream = property $ do
  s <- forAll $ stringGen
  alphas <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha
  digits <- forAll $ Gen.string (Range.linear 1 10) Gen.digit

  runParser (stream alphas) (alphas ++ s) === Just (alphas, s)

  c <- forAll $ Gen.alpha
  runParser (stream digits) (c : alphas ++ s) === Nothing
