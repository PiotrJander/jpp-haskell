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
    value <- evalExp' e1  -- a simple example of how function is a reader
    evalExp' e2 . M.insert var value


-- TODO write with reader monad

{-
-- | See examples in "Control.Monad.Reader".
-- Note, the partially applied function type @(->) r@ is a simple reader monad.
-- See the @instance@ declaration below.
class Monad m => MonadReader r m | m -> r where
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (ask | reader), local #-}
#endif
    -- | Retrieves the monad environment.
    ask   :: m r
    ask = reader id

    -- | Executes a computation in a modified environment.
    local :: (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

    -- | Retrieves a function of the current environment.
    reader :: (r -> a) -- ^ The selector function to apply to the environment.
           -> m a
    reader f = do
      r <- ask
      return (f r)

-- | Retrieves a function of the current environment.
asks :: MonadReader r m
    => (r -> a) -- ^ The selector function to apply to the environment.
    -> m a
asks = reader

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f
    reader    = id
-}


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










