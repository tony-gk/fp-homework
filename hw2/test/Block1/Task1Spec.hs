module Block1.Task1Spec
  ( testTree
  ) where

import Block1.Task1

import Control.Monad (forM_)

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

testTree :: IO TestTree
testTree = do
  unit <- unitTestTree
  return $ testGroup "Block1.stringSum" [unit, propertyBasedTestTree]

surround :: a -> [a] -> [a]
surround el list = el : list ++ [el]

unitTestTree :: IO TestTree
unitTestTree = testSpec "Unit tests" $ do

  let correctUnitSamples =
        [ (" ", 0)
        , (" 0   0 0    0  ", 0)
        , (" 0042 -10 -030  -2 ", 0)
        , ("10   2034214    502342", 2536566)
        ]

  let incorrectUnitSamples =
        [ "12 b3 0x21 "
        , "0001 12 32 _"
        , "---"
        ]

  describe "Correct samples" $ do
    forM_ correctUnitSamples $ \(sample, result) -> do
      it (surround '"' sample) $
        stringSum sample `shouldBe` Just result

  describe "Incorrect samples" $ do
    forM_ incorrectUnitSamples $ \sample -> do
      it (surround '"' sample) $
        stringSum sample `shouldBe` Nothing

buildSample :: Bool -> [[a]] -> [[a]] -> [a]
buildSample False xs (spaces : rest) = spaces ++ buildSample True xs rest
buildSample True (x : xs) spaces     = x ++ buildSample False xs spaces
buildSample True  [] _               = []
buildSample False _ []               = []

propertyBasedTestTree :: TestTree
propertyBasedTestTree = testProperty "Randomly generated tests" $ property $ do
  xs <- forAll $
    Gen.list (Range.linear 0 100) (Gen.int (Range.linear (-1000) 1000))
  spacesCount <- forAll $
    Gen.list (Range.singleton (length xs + 1)) (Gen.int (Range.linear 1 10))

  let ints = map show xs
      spaces = map (\n -> replicate n ' ') spacesCount
      sample = buildSample False ints spaces
      
  stringSum sample === Just (sum xs)
