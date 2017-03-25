module Monads where


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

{-
now let's try to use the reader Monad
-}




















