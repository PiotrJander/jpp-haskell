module Monads where

import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe
import Control.Monad.State

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

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl fun Empty
    where
        fun Empty x = Node x Empty Empty
        fun (Node v left right) x
            | x <= v = Node v (fun left x) right
            | otherwise = Node v left (fun right x)

-- start difference lists

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

-- end difference lists

toListNaive :: Tree a -> [a]
toListNaive Empty = []
toListNaive (Node x left right) = toList left ++ [x] ++ toList right

toList :: Tree a -> [a]
toList = fromDiffList . toList'

toList' :: Tree a -> DiffList a
toList' Empty = toDiffList []
toList' (Node x left right) = toDiffList [x] `mappend` toList' left `mappend` toList' right

renumber_ :: Tree a -> Tree Int
renumber_ Empty = Empty
renumber_ t = renumber_' 0 t

renumber_' :: Int -> Tree a -> Tree Int
renumber_' _ Empty = Empty
renumber_' i (Node _ t1 t2) = Node i (renumber_' (i+1) t1) (renumber_' (i+1) t2)

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

{-
b. Rozszerzmy język z poprzedniego zadania o instrukcje języka Tiny
(patrz przedmiot Semantyka i Weryfikacja Programów)

Stmt:   S ::= skip | x := e | S1;S2
        | if b then S1 else S2 | while b do S
korzystając z wcześniej napisanej funkcji evalExp, napisz funkcję

execStmt :: Stmt -> IO ()
która wykona podaną instrukcję (program) i wypisze stan końcowy
(w tym wypadku wartości zmiennych)
-}










