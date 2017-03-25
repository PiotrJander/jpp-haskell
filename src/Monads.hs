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
