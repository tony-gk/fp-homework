{-# LANGUAGE InstanceSigs #-}
module Task7 where

newtype Reader e a = Reader { runReader :: e -> a }


instance Functor (Reader e) where
  fmap f mb = do
    b <- mb
    return (f b)

instance Applicative (Reader e) where
  pure = return
  mf <*> mb = do
    f <- mf
    b <- mb
    return (f b)

instance Monad (Reader e) where
    return :: a -> Reader e a
    return a = Reader $ \_ -> a

    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    m >>= f = Reader $ \r -> runReader (f $ runReader m r) r
    
{- 

m :: Reader e a
Reader $ \r -> runReader m r == m

----left identity
return a >>= f == 
Reader $ \r -> runReader (f $ runReader (return a) r) r ==
Reader $ \r -> runReader (f a) r ==
f a

----right identity
m >>= return == 
Reader $ \r -> runReader (return $ runReader m r) r ==
Reader $ \r -> runReader (Reader $ \_ -> runReader m r) r ==
Reader $ \r -> runReader (Reader $ \_ -> runReader m r) r ==
Reader $ \r -> runReader m r ==
m

----associativity
(m >>= f) >>= g == m >>= (\x -> f x >>= g) 

(m >>= f) >>= g ==
Reader $ \r -> runReader (g $ runReader (m >>= f) r) r ==
Reader $ \r -> runReader (g $ runReader (f $ runReader m r) r) r ==

m >>= (\x -> f x >>= g) ==
Reader $ \r -> runReader ((\x -> f x >>= g) $ runReader m r) r ==
Reader $ \r -> runReader (f (runReader m r) >>= g) r ==
Reader $ \r -> runReader (g $ runReader (f $ runReader m r) r) r

-}