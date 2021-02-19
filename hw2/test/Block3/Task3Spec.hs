module Block3.Task3Spec
  ( testTree
  ) where

import Block3.Task1
import Block3.Task3

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

testTree :: IO TestTree
testTree = do
  cbs <- testCBS
  return $ testGroup "CBS and Number parsers" [cbs , propTestNumber]

testCBS :: IO TestTree
testCBS = testSpec "Correct bracket sequence parser" $ do
  let
    isCBS s = case runParser correctBracketSequence s of
      Just _  -> True
      Nothing -> False

  it "empty" $ do
    isCBS "" `shouldBe` True
  it "just pair" $ do
    isCBS "()" `shouldBe` True
  it "complex sample" $ do
    isCBS "((()()()))()()(())(())()(())" `shouldBe` True

  it "invalid symbol - fails" $ do
    isCBS "()(_)" `shouldBe` False
  it "incorrect sequence - fails" $ do
    isCBS "()(()()())()())))()()" `shouldBe` False

propTestNumber :: TestTree
propTestNumber = testProperty "Number parser" $ property $ do
  n <- forAll $ Gen.int (Range.linear minBound maxBound)
  s <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha

  runParser number (show n ++ s) === Just (n, s)
  if (n >= 0)
    then runParser number ("+" ++ show n ++ s) === Just (n, s)
    else return ()
