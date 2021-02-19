{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Task5
  ( toString 
  ) where
  
import Task4
import Data.List (intercalate)

data Var a b = Var { getVarName :: String }

toString :: ScriptExpr Var s a -> String
toString = scriptToJs 0 

scriptToJs :: Int -> ScriptExpr Var s a -> String
scriptToJs _ (SConst x) = show x

scriptToJs _ (SReadVar var) = getVarName var

scriptToJs n (SWithVar initValue body) = join
  [ "var " ++ varName ++ " = " ++ show initValue ++ ";"
  , scriptToJs (n + 1) $ body (Var varName)
  ]
  where
    varName = "v" ++ show n

scriptToJs n (SAssign var value) = 
  getVarName var ++ " = " ++ scriptToJs n value ++ ";"
  
scriptToJs n (SEq lhs rhs) = binOpToJs n lhs rhs "=="
scriptToJs n (SNotEq lhs rhs) = binOpToJs n lhs rhs "!="
scriptToJs n (SGT lhs rhs) = binOpToJs n lhs rhs ">"
scriptToJs n (SLT lhs rhs) = binOpToJs n lhs rhs "<"
scriptToJs n (SGE lhs rhs) = binOpToJs n lhs rhs ">="
scriptToJs n (SLE lhs rhs) = binOpToJs n lhs rhs "<="

scriptToJs n (SSum lhs rhs) = binOpToJs n lhs rhs "+"
scriptToJs n (SSub lhs rhs) = binOpToJs n lhs rhs "-"
scriptToJs n (SMul lhs rhs) = binOpToJs n lhs rhs "*"
scriptToJs n (SIntDiv lhs rhs) = binOpToJs n lhs rhs "/"
scriptToJs n (SFracDiv lhs rhs) = binOpToJs n lhs rhs "/"
  
scriptToJs n (SWhile cond body) = join
  [ "while (" ++ scriptToJs n cond ++ ") {"
  , tabulate (scriptToJs n body)
  , "}"
  ]

scriptToJs n (SIf cond thenBranch elseBranch) = join
  [ "if (" ++ scriptToJs n cond ++ ") {" 
  , tabulate $ scriptToJs n thenBranch
  , "} else {"
  , tabulate $ scriptToJs n elseBranch
  , "}"
  ]

scriptToJs n (SFun1 f) = let
  argName = "v" ++ show n
  resName = "v" ++ show (n + 1)
  body = f (Var argName) (Var resName)
  in join
    [ "function (" ++ argName ++ ") {"
    , tabulate $ "var " ++ resName ++ " = " ++ show (fun1DefaultRes f) ++ ";"
    , tabulate $ scriptToJs (n + 2) body
    , "\treturn " ++ resName ++ ";"
    , "}"
    ]

scriptToJs n (SFun2 f) = let
  arg1Name = "v" ++ show n
  arg2Name = "v" ++ show (n + 1)
  resName = "v" ++ show (n + 2)
  body = f (Var arg1Name) (Var arg2Name) (Var resName)
  in join
    [ "function (" ++ arg1Name ++ ", " ++ arg2Name ++ ") {"
    , tabulate $ "var " ++ resName ++ " = " ++ show (fun2DefaultRes f) ++ ";"
    , tabulate $ scriptToJs (n + 3) body
    , "\treturn " ++ resName ++ ";"
    , "}"
    ]

scriptToJs n (SNext a b) = 
  join [ scriptToJs n a, scriptToJs n b]
    
scriptToJs _ (SApply1 _ _) = 
  error "Conversion to strign for function application is undefined"
    
scriptToJs _ (SApply2 _ _ _) = 
  error "Conversion to strign for function application is undefined"

binOpToJs :: Int -> ScriptExpr Var s1 a1 -> ScriptExpr Var s2 a2 -> String -> String
binOpToJs n lhs rhs opStr = 
  "(" ++ scriptToJs n lhs ++ ") " ++ opStr ++ " (" ++ scriptToJs n rhs ++ ")"
  
fun1DefaultRes :: ScriptVar res =>
   (Var s arg -> Var s res -> ScriptExpr Var s a) -> res
fun1DefaultRes _ = defaultValue

fun2DefaultRes :: ScriptVar res => 
  (Var s arg1 -> Var s arg2 -> Var s res -> ScriptExpr Var s a) -> res
fun2DefaultRes _ = defaultValue

join :: [String] -> String
join = intercalate "\n"

tabulate :: String -> String
tabulate = join . map ('\t' :) . lines 
