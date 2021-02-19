{-# LANGUAGE FlexibleContexts #-}
module Block2.Task2
  ( moving
  ) where

import Control.Monad.State (State, evalState, get, put)
import Data.Sequence (Seq, ViewL ((:<)), viewl, (|>))
import qualified Data.Sequence as Seq (empty, length)

-- | Implementation of the Simple Moving Average Algorithm.
moving :: Fractional a => Int -> [a] -> [a]
moving n xs = evalState (mapM (movingImpl n) xs) (0, Seq.empty)

movingImpl :: Fractional a => Int -> a -> State (a, Seq a) a
movingImpl n nextVal = do
  (prevMovingAvg, prevWindow) <- get

  let 
    window = prevWindow |> nextVal
    len =  Seq.length window
    windowHead :< windowTail = viewl window
    shiftedWindow =
      if len > n then windowTail else window
    movingAvg =
      if len > n
        then prevMovingAvg + (nextVal - windowHead) / (fromIntegral n)
        else (prevMovingAvg * (fromIntegral $ len - 1) + nextVal) / (fromIntegral len)

  put (movingAvg, shiftedWindow)
  return movingAvg