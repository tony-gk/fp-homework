module Block1.Task3Spec
  ( testTree
  ) where

import Block1.Task3 as Task

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, shouldNotBe, shouldSatisfy, testSpec)

leaf :: NonEmpty Int -> Tree Int
leaf x = Node Nil x Nil

-- | Test binary search tree:
--
--                      [20]
--                     /   \
--                    /     \
--          [15,15,15]      [30, 30, 30, 30]
--          /   \                   \
--         /     \                   \
--       [3]    [17]                 [42]
--              /
--             /
--         [16, 16]
--
sampleBST :: Tree Int
sampleBST = Node
  (
  Node
    (leaf $ 3 :| [])
    (15 :| [15, 15])
    (
    Node
      (leaf $ 16 :| [16])
      (17 :| [])
      Nil
    )
  )
  (20 :| [])
  (
  Node
    Nil
    (30 :| [30, 30, 30])
    (leaf $ 42 :| [])
  )

verifyBST :: Tree Int -> Bool
verifyBST tree = verify minBound maxBound tree where
  verify _ _ Nil = True
  verify minVal maxVal (Node left (x :| _) right) =
    minVal < x
    && x < maxVal
    && verify minVal x left
    && verify x maxVal right

testTree :: IO TestTree
testTree = testSpec "Task3" $ do

  describe "isEmpty" $ do
    it "Nil" $ do
      isEmpty Nil `shouldBe` True
    it "test sample" $ do
      isEmpty sampleBST`shouldNotBe` True

  describe "size" $ do
    it "empty tree" $ do
      size Nil `shouldBe` 0
    it "node with 5 elements" $ do
      size (leaf $ 1 :| [1, 1, 1, 1]) `shouldBe` 5
    it "test sample" $ do
      size sampleBST `shouldBe` 13

  describe "find" $ do
    it "empty tree" $ do
      find 'x' Nil `shouldBe` Nothing
    it "sample containing 30" $ do
      find 30 sampleBST `shouldBe` (Just $ 30 :| [30, 30, 30])
    it "sample non containing 4" $ do
      find 4 sampleBST `shouldBe` Nothing

  describe "insert" $ do
    it "insert existing element" $ do
      insert 16 sampleBST `shouldSatisfy` (\t ->
        verifyBST t && size t - size sampleBST == 1)
    it "insert not existing element" $ do
      insert 19 sampleBST `shouldSatisfy` (\t ->
         verifyBST t && size t - size sampleBST == 1)
    it "insert 20 elements" $ do
      foldr insert sampleBST [1..20] `shouldSatisfy` (\t ->
         verifyBST t && size t - size sampleBST == 20)

  describe "fromList" $ do
    it "empty list" $ do
      fromList ([] :: [Int]) `shouldSatisfy` isEmpty
    it "list with identical elements" $ do
      let lst = take 20 $ repeat 0
      fromList lst `shouldSatisfy` \t ->
        size t == length lst
        && t == leaf (head lst :| tail lst)
    it "list with different elements" $ do
      let lst = [5..10] ++ [1..5] ++ [1..15]
      fromList lst `shouldSatisfy` \t ->
        size t == length lst
        && verifyBST t
        && all (\el -> isJust $ find el t) lst

  describe "remove" $ do
    it "insert and remove" $ do
      remove 15 (insert 15 sampleBST) `shouldSatisfy` (\t ->
        verifyBST t && size t == size sampleBST)
    it "remove non existing element" $ do
      remove 41 sampleBST `shouldBe` sampleBST
    it "remove root" $ do
      remove 20 sampleBST `shouldSatisfy` (\t ->
        verifyBST t && find 20 t == Nothing)
