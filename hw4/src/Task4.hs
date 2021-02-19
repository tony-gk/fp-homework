{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Task4
  ( ScriptVar(..)
  , ScriptExpr(..)
  , sConst
  , readVar
  , withVar
  , (@==), (@/=), (@>), (@<), (@>=), (@<=)
  , (@=), (@+), (@-), (@*), (@/), (@//)
  , while
  , sIf
  , fun1, fun2
  , apply1, apply2
  , (#)
  , interpret
  , log2
  , increment
  , euclid
  , maxDouble
  ) where

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

class (Eq v, Ord v, Show v) => ScriptVar v where
  defaultValue :: v

instance ScriptVar Int where
  defaultValue = 0

instance ScriptVar Double where
  defaultValue = 0.0

instance ScriptVar Bool where
  defaultValue = False

class (ScriptVar v, Num v) => NumScriptVar v

instance NumScriptVar Int
instance NumScriptVar Double

data ScriptExpr (ref :: * -> * -> *) s a where
  SConst   :: Show v => v ->  ScriptExpr r s v
  SReadVar :: ScriptVar v => r s v ->  ScriptExpr r s v
  SWithVar :: ScriptVar v => v -> (r s v -> ScriptExpr r s a) -> ScriptExpr r s a
  SAssign  :: ScriptVar v => r s v ->  ScriptExpr r s v -> ScriptExpr r s ()

  SEq      :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
  SNotEq   :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
  SGT      :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
  SLT      :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
  SGE      :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
  SLE      :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool

  SSum     :: NumScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v ->  ScriptExpr r s v
  SSub     :: NumScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v ->  ScriptExpr r s v
  SMul     :: NumScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v ->  ScriptExpr r s v
  SIntDiv  :: ScriptExpr r s Int -> ScriptExpr r s Int -> ScriptExpr r s Int
  SFracDiv :: ScriptExpr r s Double -> ScriptExpr r s Double -> ScriptExpr r s Double

  SWhile   :: ScriptExpr r s Bool -> ScriptExpr r s b -> ScriptExpr r s ()
  SIf      :: ScriptExpr r s Bool -> ScriptExpr r s a -> ScriptExpr r s a -> ScriptExpr r s a

  SFun1    :: (ScriptVar arg, ScriptVar res) =>
              (r s arg -> r s res -> ScriptExpr r s a) ->
              ScriptExpr r s (ScriptExpr r s arg -> ScriptExpr r s res)

  SFun2    :: (ScriptVar arg1, ScriptVar arg2, ScriptVar res) =>
              (r s arg1 -> r s arg2 -> r s res -> ScriptExpr r s a) ->
              ScriptExpr r s (ScriptExpr r s arg1 -> ScriptExpr r s arg2 -> ScriptExpr r s res)

  SApply1  :: (ScriptVar arg, ScriptVar res) =>
              ScriptExpr r s (ScriptExpr r s arg -> ScriptExpr r s res) ->
              ScriptExpr r s arg -> ScriptExpr r s res

  SApply2  :: (ScriptVar arg1, ScriptVar arg2, ScriptVar res) =>
              ScriptExpr r s (ScriptExpr r s arg1 ->ScriptExpr r s arg2 -> ScriptExpr r s res) ->
              ScriptExpr r s arg1 -> ScriptExpr r s arg2 -> ScriptExpr r s res

  SNext    :: ScriptExpr r s a -> ScriptExpr r s b -> ScriptExpr r s b

sConst :: ScriptVar v => v ->  ScriptExpr r s v
sConst = SConst

readVar :: ScriptVar v => r s v ->  ScriptExpr r s v
readVar = SReadVar

withVar :: ScriptVar v => v -> (r s v -> ScriptExpr r s a) -> ScriptExpr r s a
withVar = SWithVar

infix 4 @=
(@=) :: ScriptVar v => r s v ->  ScriptExpr r s v -> ScriptExpr r s ()
(@=) = SAssign

infix 4 @==
(@==) :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
(@==) = SEq

infix 4 @/=
(@/=) :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
(@/=) = SNotEq

infix 4 @>
(@>) :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
(@>) = SGT

infix 4 @<
(@<) :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
(@<) = SLT

infix 4 @<=
(@<=) :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
(@<=) = SLE

infix 4 @>=
(@>=) :: ScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v -> ScriptExpr r s Bool
(@>=) = SGE

infixl 6 @+
(@+) :: NumScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v ->  ScriptExpr r s v
(@+) = SSum

infixl 6 @-
(@-) :: NumScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v ->  ScriptExpr r s v
(@-) = SSub

infixl 7 @*
(@*) :: NumScriptVar v =>  ScriptExpr r s v ->  ScriptExpr r s v ->  ScriptExpr r s v
(@*) = SMul

infixl 7 @//
(@//) :: ScriptExpr r s Int -> ScriptExpr r s Int -> ScriptExpr r s Int
(@//) = SIntDiv

infixl 7 @/
(@/) :: ScriptExpr r s Double -> ScriptExpr r s Double -> ScriptExpr r s Double
(@/) = SFracDiv

while :: ScriptExpr r s Bool -> ScriptExpr r s b -> ScriptExpr r s ()
while = SWhile

sIf :: ScriptExpr r s Bool -> ScriptExpr r s a -> ScriptExpr r s a -> ScriptExpr r s a
sIf = SIf

fun1    :: (ScriptVar arg, ScriptVar res) =>
            (r s arg -> r s res -> ScriptExpr r s a) ->
            ScriptExpr r s (ScriptExpr r s arg -> ScriptExpr r s res)
fun1 = SFun1

fun2    :: (ScriptVar arg1, ScriptVar arg2, ScriptVar res) =>
            (r s arg1 -> r s  arg2 -> r s res -> ScriptExpr r s a) ->
            ScriptExpr r s (ScriptExpr r s arg1 -> ScriptExpr r s arg2 -> ScriptExpr r s res)
fun2 = SFun2

apply1  :: (ScriptVar arg, ScriptVar res) =>
            ScriptExpr r s (ScriptExpr r s arg -> ScriptExpr r s res) ->
            ScriptExpr r s arg -> ScriptExpr r s res
apply1 = SApply1

apply2  :: (ScriptVar arg1, ScriptVar arg2, ScriptVar res) =>
            ScriptExpr r s (ScriptExpr r s arg1 ->ScriptExpr r s arg2 -> ScriptExpr r s res) ->
            ScriptExpr r s arg1 -> ScriptExpr r s arg2 -> ScriptExpr r s res
apply2 = SApply2

infixl 1 #
(#) :: ScriptExpr r s a -> ScriptExpr r s b -> ScriptExpr r s b
(#) = SNext

scriptToST :: ScriptExpr STRef s a -> ST s a
scriptToST (SConst x) = return x

scriptToST (SReadVar r) = readSTRef r

scriptToST (SWithVar initValue varScope) = do
  newVar <- newSTRef initValue
  scriptToST (varScope newVar)

scriptToST (SAssign r val) = scriptToST val >>= writeSTRef r

scriptToST (SEq lhs rhs) = binOpToST lhs rhs (==)
scriptToST (SNotEq lhs rhs) = binOpToST lhs rhs (==)
scriptToST (SGT lhs rhs) = binOpToST lhs rhs (>)
scriptToST (SLT lhs rhs) = binOpToST lhs rhs (<)
scriptToST (SGE lhs rhs) = binOpToST lhs rhs (>=)
scriptToST (SLE lhs rhs) = binOpToST lhs rhs (<=)

scriptToST (SSum lhs rhs) = binOpToST lhs rhs (+)
scriptToST (SSub lhs rhs) = binOpToST lhs rhs (-)
scriptToST (SMul lhs rhs) = binOpToST lhs rhs (*)
scriptToST (SIntDiv lhs rhs) = binOpToST lhs rhs div
scriptToST (SFracDiv lhs rhs) = binOpToST lhs rhs (/)

scriptToST (SWhile cond body) = do
  bool <- scriptToST cond
  if bool
    then scriptToST body >> scriptToST (SWhile cond body)
    else return ()

scriptToST (SIf cond thenBranch elseBranch) = do
  bool <- scriptToST cond
  scriptToST $
    if bool
      then thenBranch
      else elseBranch

scriptToST (SFun1 body) = do
  resRef <- newSTRef defaultValue
  return $ \arg ->
    withVar defaultValue $ \argRef ->
    argRef @= arg #
    body argRef resRef #
    readVar resRef

scriptToST (SFun2 body) = do
  resRef <- newSTRef defaultValue
  return $ \arg1 arg2 ->
    withVar defaultValue $ \arg1Ref ->
    withVar defaultValue $ \arg2Ref ->
    arg1Ref @= arg1 #
    arg2Ref @= arg2 #
    body arg1Ref arg2Ref resRef #
    readVar resRef

scriptToST (SApply1 funScript arg) = do
  fun <- scriptToST funScript
  scriptToST $ fun arg

scriptToST (SApply2 funScript arg1 arg2) = do
  fun <- scriptToST funScript
  scriptToST $ fun arg1 arg2

scriptToST (SNext a b) = (scriptToST a) >> (scriptToST b)

binOpToST :: ScriptExpr STRef s t1 -> ScriptExpr STRef s t2 -> (t1 -> t2 -> b) -> ST s b
binOpToST lhsExpr rhsExpr op = do
  lhs <- scriptToST lhsExpr
  rhs <- scriptToST rhsExpr
  return (lhs `op` rhs)

interpret :: (forall s. ScriptExpr STRef s a) -> a
interpret script = runST $ scriptToST script

increment :: ScriptExpr r s (ScriptExpr r s Int -> ScriptExpr r s Int)
increment = fun1 $ \a res -> res @= readVar a @+ sConst 1

log2 :: ScriptExpr r s (ScriptExpr r s Int -> ScriptExpr r s Int)
log2 =
  fun1 $ \n logCnt ->
  withVar 0 $ \accum ->
    accum @= sConst 1 #
    logCnt @= sConst 0 #
    while (readVar n @> readVar accum)
      ( accum @= readVar accum @+ readVar accum #
        logCnt @= readVar logCnt @+ sConst 1
      )

euclid :: ScriptExpr r s (ScriptExpr r s Int -> ScriptExpr r s Int -> ScriptExpr r s Int)
euclid =
  fun2 $ \a b resRef ->
  withVar 0 $ \q ->
  withVar 0 $ \r ->
  while (readVar b @> sConst 0)
    ( q @= readVar a @// readVar b #
      r @= readVar a @- readVar q @* readVar b #
      a @= readVar b #
      b @= readVar r
    ) #
  resRef @= readVar a

maxDouble :: ScriptExpr r s (ScriptExpr r s Double -> ScriptExpr r s Double -> ScriptExpr r s Double)
maxDouble =
  fun2 $ \a b resRef ->
  sIf (readVar a @> readVar b)
    (resRef @= readVar a)
    (resRef @= readVar b)
