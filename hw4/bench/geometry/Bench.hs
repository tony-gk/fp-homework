{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Task1 (Point (..), doubleAreaNaive, doubleAreaStrict, perimeterNaive, perimeterStrict)

main :: IO ()
main = defaultMain
  [ perimeterBench $ 10 ^ (7 :: Int)
  , doubleAreaBench $ 10 ^ (7 :: Int)
  ]

perimeterBench :: Int -> Benchmark
perimeterBench n = 
  env (setupEnv n) $ \points -> bgroup "Perimiter"
  [ bench "Naive"  $ nf perimeterNaive points
  , bench "Strict" $ nf perimeterStrict points
  ]

doubleAreaBench :: Int -> Benchmark
doubleAreaBench n = 
  env (setupEnv n) $ \points -> bgroup "Double area"
  [ bench "Naive"  $ nf doubleAreaNaive points
  , bench "Strict" $ nf doubleAreaStrict points
  ]
  
setupEnv :: Int -> IO [Point]
setupEnv n = return $ map (\i -> Point i i) [1..n]

