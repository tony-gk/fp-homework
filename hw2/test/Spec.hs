module Main where

import Test.Tasty (TestName, TestTree, defaultMain, testGroup)

import qualified Block1.Task1Spec as B1.T1

import qualified Block2.Task1Spec as B2.T1

import qualified Block3.Task2Spec as B3.T2
import qualified Block3.Task3Spec as B3.T3
import qualified Block3.Task4Spec as B3.T4


testGroupIO :: TestName -> [IO TestTree] -> IO TestTree
testGroupIO name testsIO = sequence testsIO >>= return . testGroup name

block1 :: IO TestTree
block1 = B1.T1.testTree

block2 :: IO TestTree
block2 = B2.T1.testTree

block3 :: IO TestTree
block3 = testGroupIO "Parsers" [B3.T2.testTree, B3.T3.testTree, B3.T4.testTree]

main :: IO ()
main = defaultMain =<< testGroupIO "Homework 2 tests" [block1, block2, block3]