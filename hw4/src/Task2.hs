module Task2
  ( integrateParallel
  , integrateSequential
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parMap, rpar)
import System.Random (RandomGen, getStdGen, newStdGen, randomRs)

-- | Sequential implementation of function integration by the Monte Carlo method.
-- 'integrateSequential' @range f n@ returns integral of function @f@ 
-- in @range@ by generating @n@ points.
integrateSequential :: (Double, Double) -> (Double -> Double) -> Int -> IO Double
integrateSequential range f n = do
  gen <- getStdGen
  return $ integrateImpl range f n gen

-- | Parallel implementation of function integration by the Monte Carlo method.
-- 'integrateParallel' @range f n@ returns integral of function @f@ 
-- in @range@ by generating @n@ points.
integrateParallel :: (Double, Double) -> (Double -> Double) -> Int -> IO Double
integrateParallel range f n = do
  threads <- getNumCapabilities
  let perThread = n `div` threads
  gens <- replicateM threads newStdGen
  return $ sum $ parMap rpar (integrateImpl range f perThread) gens

integrateImpl ::
  RandomGen g => (Double, Double) -> (Double -> Double) -> Int -> g -> Double
integrateImpl range@(from, to) f n gen = let
  xs = take n $ randomRs range gen
  ys = map f xs
  in (to - from) * sum ys / (fromIntegral n)
