module Main where

import qualified Block1.Task1Spec as B1.T1
import qualified Block1.Task2Spec as B1.T2
import qualified Block1.Task3Spec as B1.T3

import qualified Block2.Task1Spec as B2.T1
import qualified Block2.Task2Spec as B2.T2

import qualified Block3.Task1Spec as B3.T1
import qualified Block3.Task2Spec as B3.T2

import Test.Tasty (TestName, TestTree, defaultMain, testGroup)

testGroupIO :: TestName -> [IO TestTree] -> IO TestTree
testGroupIO name testsIO = sequence testsIO >>= return . testGroup name

block1 :: IO TestTree
block1 = testGroupIO "Block1" [B1.T1.testTree, B1.T2.testTree, B1.T3.testTree]

block2 :: IO TestTree
block2 = testGroupIO "Block2" [B2.T1.testTree, B2.T2.testTree]

block3 :: IO TestTree
block3 = testGroupIO "Block3" [B3.T1.testTree, B3.T2.testTree]

main :: IO ()
main = defaultMain =<< testGroupIO "Homework 1 tests" [block1, block2, block3]
