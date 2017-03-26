module RunAuto where

import Auto

import System.IO
import System.Environment

{-
B. Napisz program RunAuto, taki, że wywołanie RunAuto nazwa wczyta z pliku nazwa opis automatu i słowo i odpowie True lub False w zależności czy automat akceptuje słowo (oczywiście program powinien działać dla dowolnej poprawnej nazwy pliku).

W tej części zadania stanami są liczby naturalne, alfabet składa się z liter [A-Z].

Format pliku wejściowego

liczba stanów
lista stanów startowych
lista stanów akceptujących
stan symbole stan ... stan
...
stan symbole stan ... stan
słowo
na przykład

4
[1]
[3,4]
1 C 1 2
1 AB 1
2 B 3
3 A 4
ABABABACBA
automat rozpoznaje język słów złożonych z liter A,B,C, kończących się CBA, zatem program powinien odpowiedzieć True.

Puste linie ignorujemy; w przypadku błędnego wejścia program powinien odpowiedzieć BAD INPUT (ewentualnie z komunikatem diagnostycznym)

** Wskazówki: **

Do wczytywania może się przydać funkcja readMaybe z modułu Text.Read

we'd like to use the maybe monad,
-}

main = do
    args <- getArgs
    parseFile $ head args


parseFile :: String -> IO ()
parseFile filename = do
    contents <- readFile filename
    -- TODO handle maybe
    parseContents . filter (/= "\n") . lines $ contents


parseContents :: [String] -> Maybe Bool
parseContents (states:init:final:rest) = do
    n <- readMaybe states :: Maybe Int
    initStates <- readMaybe init :: Maybe [Int]
    finalStates <- readMaybe final :: Maybe [Int]
    let
        transTable = tail rest
        word' = last rest
    transitions <- mapM parseTransition transTable
    word <- validateWord word'
    let a0 = fromLists ([0..n], initStates, finalStates, transitions)
    return $ accepts a0 word


parseTransition :: String -> Maybe (Int, Char, [Int])
parseTransition s = do
    from <- readMaybe a :: Maybe Int
    sym <- readMaybe b :: Maybe Char
    dest <- mapM (\x -> readMaybe x :: Maybe Int) (words rest)
    return (from, sym, dest)
    where
        (a:b:rest) = words s


validateWord :: String -> Maybe String
validateWord s
    | all ((&&) <$> (== 'A') <*> (== 'B') <*> (== 'C')) word = return word
    | otherwise = mempty
    where
        word = unwords . words $ s









