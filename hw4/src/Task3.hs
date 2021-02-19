{-# LANGUAGE BangPatterns #-}

module Task3
  ( ConcurrentHashTable(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.Monad (void, when)
import Data.Hashable (Hashable, hash)
import Data.Maybe (isNothing)
import Data.Vector ((!))
import qualified Data.Vector as V (Vector, forM_, generateM, length)

type CoreElement k v = TVar (Maybe (k, v))

type Core k v = V.Vector (CoreElement k v)

data ConcurrentHashTable k v =
  CHT { coreVar :: TVar (Core k v)  -- ^ 
      , sizeVar :: TVar Int
      , expandingCore :: TVar Bool
      }

initSize :: Int
initSize = 4

growthFactor :: Int
growthFactor = 2

newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  initCore <- V.generateM initSize (const $ newTVar Nothing)
  initCoreVar <- newTVar initCore
  initSizeVar <- newTVar 0
  initExpandingCore <- newTVar False
  return $ CHT initCoreVar initSizeVar initExpandingCore

getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key cht = atomically $ do
  core <- readTVar (coreVar cht)
  let coreSize = V.length core
      idx = hash key `mod` coreSize
      indices = [idx..coreSize - 1] ++ [0..idx - 1]
  findElem core key indices

findElem :: Eq k => Core k v -> k -> [Int] -> STM (Maybe v)
findElem _ _ [] = return Nothing
findElem core key (i : rest) = do
  el <- readTVar (core ! i)
  case el of
    Just (k, v) -> if (k == key)
      then return $ Just v
      else findElem core key rest
    Nothing -> return Nothing

putCHT :: (Hashable k, Eq k, Show k, Show v) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value cht = atomically $ do
  capacity <- V.length <$> readTVar (coreVar cht)
  size <- readTVar (sizeVar cht)
  when (capacity == size) $ do
    isExpanding <- readTVar (expandingCore cht)
    if isExpanding
      then retry
      else expandTable cht
  core <- readTVar (coreVar cht) 
  isNewKey <- putCore key value core
  when isNewKey $ writeTVar (sizeVar cht) (size + 1)

expandTable :: (Hashable k, Eq k, Show k, Show v) => ConcurrentHashTable k v -> STM ()
expandTable cht = do
  writeTVar (expandingCore cht) True
  core <- readTVar (coreVar cht)
  size <- readTVar (sizeVar cht)
  newCore <- V.generateM (size * growthFactor) (const $ newTVar Nothing)
  V.forM_ core $ \elVar -> do
    el <- readTVar elVar
    case el of
      Just (k, v) -> void $ putCore k v newCore
      Nothing     -> return ()
  writeTVar (coreVar cht) newCore
  writeTVar (expandingCore cht) False

putCore :: (Hashable k, Eq k) => k -> v -> Core k v -> STM Bool
putCore key value core = do
  let coreSize = V.length core
      idx = hash key `mod` coreSize
      indices = [idx..coreSize - 1] ++ [0..idx - 1]
  placeVar <- findPlaceToPut core key indices
  was <- readTVar placeVar
  writeTVar placeVar (Just (key, value))
  return $ isNothing was

findPlaceToPut :: Eq k => Core k v -> k -> [Int] -> STM (TVar (Maybe (k, v)))
findPlaceToPut _ _ [] = error "Internal error: failed to find place to put element."
findPlaceToPut core key (i : rest) = do
  let elVar = core ! i
  el <- readTVar (core ! i)
  case el of
    Just (k, _) -> if k == key
      then return elVar
      else findPlaceToPut core key rest
    Nothing -> return elVar


sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT cht = atomically $ readTVar (sizeVar cht)
