module RunAuto where

import Auto

import System.IO
import System.Environment
import Text.Read
import Control.Monad

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
-}

-- main :: IO ()
-- main = readModel


readModel :: IO ()
readModel = do
    args <- getArgs
    parseFile $ head args


parseFile :: String -> IO ()
parseFile filename = do
    contents <- readFile filename
    let result = parseContents . filter (\line -> (unwords . words $ line) /= "") . lines $ contents
    case result of {
        Left msg -> putStrLn $ "BAD INPUT: " ++ msg;
        Right b -> print b
    }


parseContents :: [String] -> Either String Bool
parseContents (states:init:final:rest) = do
    n <- (readMaybe states :: Maybe Int) >>> "parseContents: " ++ states
    initStates <- (readMaybe init :: Maybe [Int]) >>> "parseContents: " ++ init
    finalStates <- (readMaybe final :: Maybe [Int]) >>> "parseContents: " ++ final
    when (null rest) (Left $ "parseContents: " ++ "word not given")
    let
        transTable = Prelude.init rest
        word' = last rest
    transitions <- concat <$> mapM parseTransition transTable
    word <- validateWord word'
    let a0 = fromLists [0..n] initStates finalStates transitions
    return $ accepts a0 word


parseTransition :: String -> Either String [(Int, Char, [Int])]
parseTransition s = do
    case words s of {
        (a:b:rest) -> return ();
        _ -> Left $ "parseTransition: " ++ s
    }
    let (a:b:rest) = words s
    from <- (readMaybe a :: Maybe Int) >>> "parseTransition: " ++ s
    dest <- mapM (\x -> (readMaybe x :: Maybe Int) >>> "parseTransition: " ++ s) rest
    return $ map (\sym -> (from, sym, dest)) b


validateWord :: String -> Either String String
validateWord s
    | all (\c -> (c == 'A') || (c == 'B') || (c == 'C')) word = return word
    | otherwise = Left $ "validateWord " ++ word
    where
        word = unwords . words $ s


infixl 4 >>>
(>>>) :: Maybe a -> String -> Either String a
Nothing >>> msg = Left msg
(Just x) >>> msg = Right x






