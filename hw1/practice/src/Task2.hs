module Task2 where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M

data Expr a
  = Const a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)

type VarMap a = Map String a

evalExpr :: Num a => Expr a -> VarMap a -> a
evalExpr e vars = runReader (eval e) vars

eval :: Num b => Expr b -> Reader (VarMap b) b
eval (Const a) = return a

eval (Var name) = do
  vars <- ask
  return $ case M.lookup name vars of
    Just val -> val
    Nothing  -> error ("undefined variable " ++ name)
    
eval (Add e1 e2) = evalBinary e1 e2 (+)
eval (Sub e1 e2) = evalBinary e1 e2 (-)
eval (Mul e1 e2) = evalBinary e1 e2 (*)

evalBinary :: Num a => Expr a -> Expr a -> (a -> a -> a) -> Reader (VarMap a) a
evalBinary e1 e2 op = do
  val1 <- eval e1
  val2 <- eval e2
  return (val1 `op` val2)