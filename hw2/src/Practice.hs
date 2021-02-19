{-# LANGUAGE RankNTypes #-}
module Practice where

import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Functor.Const (Const (Const, getConst))


type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
-- type Lens' s a  = Lens s s a a
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s


----------- #5
over :: Lens' s a -> (a -> a) -> s -> s
over lns modifier s = runIdentity $ lns (Identity . modifier) s

view :: Lens' s a -> s -> a
view lns s = getConst $ lns Const s

set  :: Lens' s a -> a -> s -> s
set lns a = over lns (const a)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. lns = view lns s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over





----------- #6

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 a2fb (a, x) = fmap (\b -> (b, x)) $ a2fb a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 a2fb (x, a) = fmap (\b -> (x, b)) $ a2fb a




----------- #7

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \a2fb s -> fmap (\b -> setter s b) $ a2fb (getter s)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _  a2fb (Left s1)  = Left <$> l1 a2fb s1
choosing _  l2 a2fb (Right s2) = Right <$> l2 a2fb s2

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l (\a -> (f a, f a)) s

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (\a -> (a, f a)) s
