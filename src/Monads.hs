module Monads where

import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader

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

-- instance Functor Tree where
--     fmap _ Empty = Empty
--     fmap f (Node x left right) = Node (f x) left right

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl fun Empty
    where
        fun Empty x = Node x Empty Empty
        fun (Node v left right) x
            | x <= v = Node v (fun left x) right
            | otherwise = Node v left (fun right x)

-- -- start difference lists
--
-- newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
--
-- toDiffList :: [a] -> DiffList a
-- toDiffList xs = DiffList (xs++)
--
-- fromDiffList :: DiffList a -> [a]
-- fromDiffList (DiffList f) = f []
--
-- instance Monoid (DiffList a) where
--     mempty = DiffList (\xs -> [] ++ xs)
--     (DiffList f) `mappend` (DiffList g) = DiffList (f . g)
--
-- -- end difference lists

toListNaive :: Tree a -> [a]
toListNaive Empty = []
toListNaive (Node x left right) = toListNaive left ++ [x] ++ toListNaive right

-- toList :: Tree a -> [a]
-- toList = fromDiffList . toList'
--
-- toList' :: Tree a -> DiffList a
-- toList' Empty = toDiffList []
-- toList' (Node x left right) = toDiffList [x] `mappend` toList' left `mappend` toList' right

renumber_ :: Tree a -> Tree Int
renumber_ Empty = Empty
renumber_ t = renumber_' 0 t

renumber_' :: Int -> Tree a -> Tree Int
renumber_' _ Empty = Empty
renumber_' i (Node _ t1 t2) = Node i (renumber_' (i+1) t1) (renumber_' (i+1) t2)



{-
the env is the level number

reader monad is all about making a function composition tree
where the tree depends on the env

here we can bind to nonexistent (yet) computations
-}


renumber :: Tree a -> Tree Int
renumber tree = renumber'' tree 0

renumber' :: Tree a -> Reader Int (Tree Int)
renumber' Empty = return Empty
renumber' (Node x left right) = do
    i <- ask  -- nice for binding; nice for composing functions; a bit like let
--     a <- reader (+1)  -- like ask, but also applies a function
--     a <- local (+5) $ do  -- example with nested reader
--         a <- ask
--         let b = a + 1
--         return b
    le <- local (+1) $ renumber' left
    ri <- local (+1) $ renumber' right
    return $ Node i le ri

renumber'' :: Tree a -> Int -> Tree Int
renumber'' Empty = return Empty
renumber'' (Node x left right) = do
    i <- ask
    le <- local (+1) $ renumber'' left
    ri <- local (+1) $ renumber'' right
    return $ Node i le ri




type Var = String
type Env = M.Map Var Int

data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2
--      | ENot Exp

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
-- evalExp' (ENot exp) = do
--     result <- evalExp' exp
--     return $ if result == 0 then 1 else 0
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

{-
a. Dany typ drzew

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
Napisz funkcję

renumberTree :: Tree a -> Tree Int
która ponumeruje wezly drzewa tak, ze kazdy z nich bedzie mial inny numer.
Porownaj rozwiazania z uzyciem monady State i bez.

możliwe dodatkowe wymaganie: ponumeruj wezly drzewa w kolejnosci infiksowej.

(toList $ renumber $ fromList "Learn Haskell") == [0..12]
-}

renumberTree :: Tree a -> Tree Int
renumberTree tree = evalState (renumberTree' tree) 0

renumberTree' :: Tree a -> State Int (Tree Int)
renumberTree' Empty = return Empty
renumberTree' (Node _ left right) = do
    i <- get
    put $ i + 1
    le <- renumberTree' left
    ri <- renumberTree' right
    return $ Node i le ri








