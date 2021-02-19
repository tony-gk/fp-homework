module Task6 where

newtype Writer w a = Writer { runWriter :: (a, w) } 

instance (Monoid w) => Functor (Writer w) where
  fmap f mb = do
    b <- mb
    return (f b)

instance (Monoid w) => Applicative (Writer w) where
  pure = return
  mf <*> mb = do
    f <- mf
    b <- mb
    return (f b)
    
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    m >>= f = Writer (y, w1 <> w2)
      where
        (x, w1) = runWriter m
        (y, w2) = runWriter (f x)
      

{- 


----left identity
return a >>= f == 
Writer (y, mempty <> w2) == Writer (y, w2) == f a
  where
    (a, mempty) = runWriter (return a)
    (y, w2) = runWriter (f a)


----right identity
m >>= return == 
Writer (x, w1 <> mempty) == Writer (x, w1) == m
  where
    (x, w1) = runWriter m
    (x, mempty) = runWriter (return x)

----associativity
(m >>= f) >>= g == m >>= (\x -> f x >>= g) 

(m >>= f) >>= g ==
Writer (y, w1 <> w2) >>= g
  where 
    (x, w1) = runWriter m
    (y, w2) = runWriter (f x)
== Writer (z, w1 <> w2 <> w3)
  where 
    (z, w3) = runWriter (g y)

m >>= (\x -> f x >>= g) ==
Writer (z, w1 <> w2 <> w3)
  where
    (x, w1) = runWriter m
    (z, w2 <> w3) = runWriter $ f x >>= g
      where
        (y, w2) = runWriter (f x)
        (z, w3) = runWriter (g y)
-}