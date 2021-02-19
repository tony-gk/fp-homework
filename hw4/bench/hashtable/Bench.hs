module Main where

import Control.Concurrent.Async (async, wait)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (forM, forM_, replicateM, void)
import System.Random (newStdGen, randomIO, randomRIO, randomRs)

import Criterion.Main

import Task3 (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)

keyRange :: (Int, Int)
keyRange = (1, 1000)

opCount :: Int
opCount = 10 ^ (6 :: Int)

main :: IO ()
main = defaultMain [ hashtableBench ]

hashtableBench :: Benchmark
hashtableBench = bgroup ((show opCount) ++ " operations")
  [ concurrentBench 1
  , concurrentBench 2
  , concurrentBench 3
  , concurrentBench 4
  ]

concurrentBench :: Int -> Benchmark
concurrentBench threads =
  env setEnv $ \threadsOps ->
    bench (show threads ++ " threads") $
      nfIO (newCHT >>= performConcurrently threadsOps)
  where
    setEnv = do
      let perThread = opCount `div` threads
      replicateM threads (genOperations perThread)

data CHTOperation = Put Int Int | Get Int | Size

instance NFData CHTOperation where
  rnf (Put a b) = a `deepseq` b `deepseq` ()
  rnf (Get a)   = a `deepseq` ()
  rnf Size      = ()

genOperations :: Int -> IO [CHTOperation]
genOperations n = do
  rng <- newStdGen
  let xs = take n $ randomRs (1 :: Int, 5) rng
  mapM f xs
  where
    f 1 = genGetOperation
    f 2 = return Size
    f _ = genPutOperation

genPutOperation :: IO CHTOperation
genPutOperation = do
  key <- randomRIO keyRange
  value <- randomIO
  return $ Put key value

genGetOperation :: IO CHTOperation
genGetOperation = do
  key <- randomRIO keyRange
  return $ Get key

performOperations :: [CHTOperation] -> ConcurrentHashTable Int Int -> IO ()
performOperations ops hashtable = do
  forM_ ops $ \op -> case op of
    Put key val -> putCHT key val hashtable
    Get key     -> void $ getCHT key hashtable
    Size        -> void $ sizeCHT hashtable

performConcurrently :: [[CHTOperation]] -> ConcurrentHashTable Int Int -> IO ()
performConcurrently threadsOps hashtable = do
  as <- forM threadsOps $ \ops ->
    async $ performOperations ops hashtable
  forM_ as wait
