module Block3.Task2Spec
  ( testTree
  ) where

import Block3.Task2

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

import Data.Foldable (forM_)

thisOrThatTriples :: [[ThisOrThat Int Int]]
thisOrThatTriples = do
  let list = [This 1, That 2, Both 3 4]
  a <- list
  b <- list
  c <- list
  return [a, b, c]

testTree :: IO TestTree
testTree = testSpec "Task2" $ do
  describe "NonEmpty" $ do
    let a = "a-a"
        b = "b-b"
        c = "c-c"
    it "associativity law" $ do
      (a <> b) <> c `shouldBe` a <> (b <> c)
      (c <> a) <> b `shouldBe` c <> (a <> b)

  describe "ThisOrThat" $ do
    it "associativity law" $ do
      forM_ thisOrThatTriples $ \[a, b, c] ->
        (a <> b) <> c `shouldBe` a <> (b <> c)

  describe "Name" $ do
    let a = Name "ru"
        b = Name "ifmo"
        c = Name "rain"
    it "mappend joins name with a dot" $ do
      a <> b <> c `shouldBe` Name "ru.ifmo.rain"
    it "left identity law" $ do
      mempty <> a `shouldBe` a
    it "right identity law" $ do
      b <> mempty `shouldBe` b
    it "associativity law" $ do
      (a <> b) <> c `shouldBe` a <> (b <> c)

  describe "Endo" $ do
    let f = Endo (+1)
        g = Endo (*5)
        h = Endo (^(2 ::Int))
        arg = 19 :: Int
        f1 `shouldBeSimilar` f2 = getEndo f1 arg `shouldBe` getEndo f2 arg
    it "left identity law" $ do
      mempty <> f `shouldBeSimilar` f
    it "right identity law" $ do
      (g <> mempty) `shouldBeSimilar` g
    it "associativity law" $ do
      ((f <> g) <> h) `shouldBeSimilar` (f <> (g <> h))
