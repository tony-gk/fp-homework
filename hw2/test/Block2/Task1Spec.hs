module Block2.Task1Spec
  ( testTree
  ) where

import Block2.Task1

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

testTree :: IO TestTree
testTree = testSpec "Block2.Arithmetic expressions" $ do
  it "Const" $
    eval (Const 42) `shouldBe` Right 42
  it "Add" $ do
    eval (Add (Const 12) (Const (-10))) `shouldBe` Right 2
  it "Sub" $ do
    eval (Sub (Const 15) (Const 16)) `shouldBe` Right (-1)

  describe "Div" $ do
    it "x / 1 == x" $ do
      eval (Div (Const 1521) (Const 1)) `shouldBe` Right 1521
    it "0 / x == 0" $ do
      eval (Div (Const 0) (Const 9999)) `shouldBe` Right 0
    it "div by zero error" $ do
      eval (Div (Const 2134) (Const 0)) `shouldBe` Left DivisionByZero

  describe "Mult" $ do
    it "0 * x == 0" $ do
      eval (Mult (Const 0) (Const 112)) `shouldBe` Right 0
    it "x * 1 == x" $ do
      eval (Mult (Const 134) (Const 1)) `shouldBe` Right 134
    it "10*10 == 100" $ do
      eval (Mult (Const 10) (Const 10)) `shouldBe` Right 100

  describe "Exp" $ do
    it "x^0 == 1" $ do
      eval (Exp (Const 8123) 0) `shouldBe` Right 1
    it "negative exp error" $ do
      eval (Exp (Const 2) (-5)) `shouldBe` Left NegativeExponent
