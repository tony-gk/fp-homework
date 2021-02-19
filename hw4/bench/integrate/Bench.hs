module Main where

import Criterion.Main
import Task2 (integrateParallel, integrateSequential)

main :: IO ()
main = defaultMain
  [ integrateBench $ 10 ^ (6 :: Int) ]

integrateBench :: Int -> Benchmark
integrateBench n = bgroup "Integrate"
  [ bench "Sequential" $ nfIO (integrateSequential range f n)
  , bench "Parallel" $ nfIO (integrateParallel range f n)
  ]
  where
    range = (0.0, 100.0)
    f x = cos x ^ (2 :: Int) - sin x ^ (3 :: Int)
    