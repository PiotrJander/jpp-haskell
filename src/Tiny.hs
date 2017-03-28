module Tiny where

import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader


type Var = String
type Env = M.Map Var Int

data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub | OpMod | OpEq | OpNeq

infix 6 ==:
(==:) :: Exp -> Exp -> Exp
e1 ==: e2 = EOp OpEq e1 e2

infix 6 /=:
(/=:) :: Exp -> Exp -> Exp
e1 /=: e2 = EOp OpNeq e1 e2

infix 7 +:
(+:) :: Exp -> Exp -> Exp
e1 +: e2 = EOp OpAdd e1 e2

infix 7 -:
(-:) :: Exp -> Exp -> Exp
e1 -: e2 = EOp OpSub e1 e2

infix 8 *:
(*:) :: Exp -> Exp -> Exp
e1 *: e2 = EOp OpMul e1 e2

infix 9 %:
(%:) :: Exp -> Exp -> Exp
e1 %: e2 = EOp OpMod e1 e2

data Stmt = Skip | Stmt :/ Stmt | Var := Exp | IfThenElse Exp Stmt Stmt | WhileDo Exp Stmt
infixl 4 :/
infix 5 :=


evalExpNoContext :: Exp -> Int
evalExpNoContext exp = runReader (evalExp' exp) M.empty

evalExp :: Exp -> Env -> Int
evalExp exp = runReader (evalExp' exp)

evalExp' :: Exp -> Reader Env Int
evalExp' (EInt i) = return i
evalExp' (EOp OpAdd e1 e2) = liftM2 (+) (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpMul e1 e2) = liftM2 (*) (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpSub e1 e2) = liftM2 (-) (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpMod e1 e2) = liftM2 mod (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpEq e1 e2) = liftM2 (\f1 f2 -> if f1 == f2 then 1 else 0) (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpNeq e1 e2) = liftM2 (\f1 f2 -> if f1 /= f2 then 1 else 0) (evalExp' e1) (evalExp' e2)
evalExp' (EVar var) = do
    justValue <- reader $ M.lookup var
    return $ let (Just value) = justValue in value
evalExp' (ELet var e1 e2) = do
    value <- evalExp' e1
    local (M.insert var value) (evalExp' e2)


-- | Execute the program and print final variable values
execStmt :: Stmt -> IO ()
execStmt = execStmtWithArgs M.empty

execStmtWithArgs :: Env -> Stmt -> IO ()
execStmtWithArgs env stmt = mapM_ printVar . M.toList . execState (execStmtState stmt) $ env
    where
        printVar (k, v) = putStrLn $ k ++ " = " ++ show v

execStmtState :: Stmt -> State Env ()
execStmtState Skip = modify id
execStmtState (stmt1 :/ stmt2) = do
    execStmtState stmt1
    execStmtState stmt2
execStmtState (var := exp) = do
    result <- gets $ evalExp exp
    modify $ M.insert var result
execStmtState (IfThenElse cond then_ else_) = do
    test <- gets $ evalExp cond
    if test == 1 then execStmtState then_ else execStmtState else_
execStmtState (WhileDo cond stmt) = do
    test <- gets $ evalExp cond
    when (test == 1) $ do
        execStmtState stmt
        execStmtState (WhileDo cond stmt)

-- | Depends on args "a" and "a" in the env
gcd' :: Stmt
gcd' = WhileDo (EVar "a" /=: EInt 0) (
    "c" := EVar "a" :/
    "a" := EVar "b" %: EVar "a" :/
    "b" := EVar "c"
    ) :/
    "result" := EVar "b" :/
    IfThenElse (EVar "result" ==: EInt 1) ("coprime" := EInt 1) Skip

args :: Int -> Int -> Stmt -> IO ()
args a b = execStmtWithArgs (M.fromList [("a", a), ("b", b)])

--
--      let x =
--          let y = 6 + 9
--          in y - 1
--      in x * 3
--
-- ==>  42
--
e1 = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"
