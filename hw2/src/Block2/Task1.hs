module Block2.Task1
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  ) where


import Control.Monad (when)
-- | Data type representing expressions.
data Expr
  -- | Constant value
  = Const Int
  -- | Addition
  | Add Expr Expr
  -- | Substraction
  | Sub Expr Expr
  -- | Multiplication
  | Mult Expr Expr
  -- | Division
  | Div Expr Expr
  -- | Exponentiation
  | Exp Expr Int
  deriving (Show, Eq)

-- | Data type representing arithmetic errors.
data ArithmeticError
  = DivisionByZero
  | NegativeExponent
  deriving (Show, Eq)

-- | Evaluates 'Expr'.
eval :: Expr -> Either ArithmeticError Int
eval (Const x) = return x

eval (Add e1 e2) = evalBinaryExpr e1 e2 (+)
eval (Sub e1 e2) = evalBinaryExpr e1 e2 (-)
eval (Mult e1 e2) = evalBinaryExpr e1 e2 (*)

eval (Div e1 e2) = do
  x <- eval e1
  y <- eval e2
  when (y == 0) $ Left DivisionByZero
  return (x `div` y)

eval (Exp e power) = do
  when (power < 0) $ Left NegativeExponent
  x <- eval e
  return (x ^ power)

evalBinaryExpr :: Expr -> Expr -> (Int -> Int -> Int) -> Either ArithmeticError Int
evalBinaryExpr e1 e2 op = do
  x <- eval e1
  y <- eval e2
  return (x `op` y)
