module Monads where

import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe

-- problem 1

allPairs' :: [a] -> [a] -> [[a]]
allPairs' xs ys = [[x,y] | x <- xs, y <- ys]

allPairs :: [a] -> [a] -> [[a]]
allPairs xs ys = do
    x <- xs
    y <- ys
    return [x, y]

allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
allCombinations (xs:xss) = do
    y <- xs
    ys <- allCombinations xss
    return (y : ys)


-- problem 2

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumber_ :: Tree a -> Tree Int
renumber_ Empty = Empty
renumber_ t = renumber_' 0 t

renumber_' :: Int -> Tree a -> Tree Int
renumber_' _ Empty = Empty
renumber_' i (Node _ t1 t2) = Node i (renumber_' (i+1) t1) (renumber_' (i+1) t2)

t :: Tree Int
t = Node 0 (Node 0 Empty Empty) (Node 0 Empty (Node 0 Empty Empty))


-- renumber :: Tree a -> Tree Int
-- renumber


type Var = String
type Env = M.Map Var Int

data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

-- | We could have an error when a variable not in the env, but we ignore this
evalExp :: Exp -> Int
evalExp exp = evalExp' exp M.empty

-- evalExp' (EOp OpAdd e1 e2) = do
--     fe1 <- evalExp' e1
--     fe2 <- evalExp' e2
--     return $ fe1 + fe2

evalExp' :: Exp -> Env -> Int
evalExp' (EInt i) = return i
evalExp' (EOp OpAdd e1 e2) = liftM2 (+) (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpMul e1 e2) = liftM2 (*) (evalExp' e1) (evalExp' e2)
evalExp' (EOp OpSub e1 e2) = liftM2 (-) (evalExp' e1) (evalExp' e2)
evalExp' (EVar var) = \env -> let (Just value) = M.lookup var env in value
evalExp' (ELet var e1 e2) = do
    value <- evalExp' e1
    evalExp' e2 . M.insert var value


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






