module Task2 where

data Expr a
  = Const a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)

data Statement a
  = Seq (Statement a) (Statement a)
  | EvalAndPrint (Expr a)
  | Log String

evalStatement :: (Show a, Num a) => Statement a -> String
evalStatement (Log s) = s
evalStatement (EvalAndPrint e) = show $ evalExpr e
evalStatement (Seq e1 e2) = (evalStatement e1) ++ "\n" ++ (evalStatement e2)

evalExpr :: Num a => Expr a -> a
evalExpr (Const x) = x
evalExpr (Add e1 e2) = evalBinaryExpr e1 e2 (+)
evalExpr (Sub e1 e2) = evalBinaryExpr e1 e2 (-)
evalExpr (Mul e1 e2) = evalBinaryExpr e1 e2 (*)

evalBinaryExpr :: (Num a, Num b) => Expr a -> Expr b -> (a -> b -> c) -> c
evalBinaryExpr e1 e2 op = (evalExpr e1) `op` (evalExpr e2)