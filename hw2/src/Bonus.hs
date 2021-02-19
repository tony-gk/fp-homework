{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Bonus
  ( Cont (..)
  , Paused (..)
  , ExitStatus (..)
  , Process
  , writeLine
  , readLine
  , fork
  , yield
  , exit
  , kernel
  ) where

import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

-- | Continuation monad.
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f conta = Cont $ \k -> runCont conta (k . f)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont $ \k -> k a

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  contf <*> conta = Cont $ \k ->
    runCont conta $ \a ->
    runCont contf $ \a2b ->
    k (a2b a)

instance Monad (Cont r) where
  return :: a -> Cont r a
  return = pure

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  conta >>= f = Cont $ \k -> runCont conta (\a -> runCont (f a) k)

type Process = Cont Paused ()

type KernelAction = ReaderT (IORef [Paused]) IO ()

class SyscallTag tag where
  type SyscallArgument tag :: *
  type SyscallResult   tag :: *

  kernelAction :: tag -> SyscallArgument tag -> (SyscallResult tag -> Paused) -> KernelAction

data Paused = forall st. SyscallTag st => Paused
  { _tag  :: st
  , _arg  :: SyscallArgument st
  , _cont :: (SyscallResult st) -> Paused }

syscall :: SyscallTag st => st -> SyscallArgument st -> Cont Paused (SyscallResult st)
syscall syscallTag syscallArg = Cont $ \k -> Paused syscallTag syscallArg k

-- | Tag responsible for the system call to read the line.
data ReadTag = ReadTag

instance SyscallTag ReadTag where
  type SyscallArgument ReadTag = ()
  type SyscallResult   ReadTag = String

  kernelAction ReadTag () cont = do
    input <- liftIO $ getLine
    modify ([cont input] ++)

-- | Reads a line.
readLine :: Cont Paused String
readLine = syscall ReadTag ()

-- | Tag responsible for the system call to write the line.
data WriteTag = WriteTag

instance SyscallTag WriteTag where
  type SyscallArgument WriteTag = String
  type SyscallResult   WriteTag = ()

  kernelAction WriteTag s cont = do
    liftIO $ putStrLn s
    modify ([cont ()] ++)

-- | Writes a line.
writeLine :: String -> Cont Paused ()
writeLine s = syscall WriteTag s

-- | Tag responsible for the yield system call.
data YieldTag = YieldTag

instance SyscallTag YieldTag where
  type SyscallArgument YieldTag = ()
  type SyscallResult   YieldTag = ()

  kernelAction YieldTag _ cont = do
    modify (++ [cont ()])

-- | Suspends a running process and transfers control
-- to the next process in the queue. The current process is
-- added to the end of the queue.
yield :: Cont Paused ()
yield = syscall YieldTag ()

-- | Tag responsible for the fork system call.
data ForkTag = ForkTag

instance SyscallTag ForkTag where
  type SyscallArgument ForkTag = Process
  type SyscallResult   ForkTag = ()

  kernelAction ForkTag p cont = do
    modify ([processToPaused p, cont ()] ++)

-- | @fork p@ runs process @p@. The current process is
-- added to the queue after process @p@.
fork :: Process -> Cont Paused ()
fork p = syscall ForkTag p

-- | Tag responsible for the exit system call.
data ExitTag = ExitTag

-- | Exit status type.
data ExitStatus = Success | Failed
  deriving Show

instance SyscallTag ExitTag where
  type SyscallArgument ExitTag = ExitStatus
  type SyscallResult   ExitTag = ()

  kernelAction ExitTag status _ = do
    liftIO $ print status

-- | Terminates the current process. Each process must
-- end with this statement.
exit :: ExitStatus -> Cont Paused ()
exit status = syscall ExitTag status

-- | Converts a 'Process' to the 'IO' monad.
kernel :: Process -> IO ()
kernel process = do
  runnablesRef <- newIORef [processToPaused process]
  runReaderT kernelIO runnablesRef

kernelIO :: KernelAction
kernelIO = do
  runnablesRef <- ask
  runnables <- liftIO $ readIORef runnablesRef
  case runnables of
    [] -> return ()
    (Paused tag arg cont) : rest -> do
      liftIO $ writeIORef runnablesRef rest
      kernelAction tag arg cont
      kernelIO

processToPaused :: Process -> Paused
processToPaused p = runCont p (error "Expected exit as a terminated statement")

modify :: (a -> a) -> ReaderT (IORef a) IO ()
modify f = do
  ref <- ask
  liftIO $ modifyIORef ref f
