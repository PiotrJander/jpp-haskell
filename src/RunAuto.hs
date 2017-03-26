module RunAuto where

import Auto

import System.IO

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
-}

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
















