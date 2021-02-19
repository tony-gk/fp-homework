module Block3.Task1Spec
  ( testTree
  ) where

import Block3.Task1

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

import Data.Monoid

testTree :: IO TestTree
testTree = testSpec "Task1" $ do
  describe "maybeConcat" $ do
    it "empty list" $ do
      maybeConcat [] `shouldBe` ""
    it "onle Nothing elements" $ do
      maybeConcat (replicate 10 Nothing) `shouldBe` ""
    it "mixed Just and Nothing" $ do
      maybeConcat [Nothing, Just "abc", Nothing, Nothing, Just "cde", Nothing, Just "xx"]
        `shouldBe` "abccdexx"

  describe "eitherConcat" $ do
    it "empty list" $ do
      eitherConcat [] `shouldBe` ("", "")
    it "only Left elements" $ do
      eitherConcat [Left (Sum 3), Left (Sum 1), Left (Sum 15)]
        `shouldBe` (Sum (19 :: Int), "")
    it "only Right elements" $ do
      eitherConcat [Right (Product 1), Right (Product 2), Right (Product 3)]
        `shouldBe` ("", Product (6 :: Int))
    it "mixed Left and Right" $ do
      eitherConcat [Right "R", Left "Le", Right "i", Right "g", Left "f", Left "t", Right "ht"]
        `shouldBe` ("Left", "Right")
