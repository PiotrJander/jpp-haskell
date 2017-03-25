module Auto (
    Auto
    , accepts
    , emptyA
    , epsA
    , symA
    , leftA
    , sumA
    , thenA
    , fromLists
    , toLists
) where

import Data.List

{-
transition could be specified by a table Q x Sigma
if char not in the transition row, then go to the dead state
here we could adopt some sort of convention for the dead state
-}

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

-- | accepts aut w mówi czy automat aut akceptuje słowo w
-- | Funkcja accepts powinna działać w czasie liniowym zwn. długość słowa.
accepts :: Eq q => Auto a q -> [a] -> Bool
accepts = undefined


-- | emptyA rozpoznaje język pusty
emptyA :: Auto a ()
emptyA = A {
    states = [()],
    initStates = [()],
    isAccepting = \_ -> False,
    transition = \_ _ -> []
}


-- | epsA rozpoznaje język złożony ze słowa pustego
epsA :: Auto a ()
epsA = undefined


-- | symA c rozpoznaje język {c}
symA :: Eq a => a -> Auto a Bool
symA = undefined


-- | leftA aut rozpoznaje ten sam język co aut
leftA :: Auto a q -> Auto a (Either q r)
leftA = undefined


-- | język automatu sumA aut1 aut2 jest sumą języków dla aut1 i aut2
sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA = undefined


-- | język automatu thenA aut1 aut2 jest konkatenacją języków dla aut1 i aut2
thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA = undefined


-- | fromLists tłumaczy pomiędzy reprezentacją funkcyjną a reprezentacją listową (stany,startowe,akceptujące,przejścia)
fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists = undefined


-- | toLists tłumaczy pomiędzy reprezentacją funkcyjną a reprezentacją listową (stany,startowe,akceptujące,przejścia)
toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists = undefined


-- instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
{-
warto przy tym zadbać aby wypisywana reprezentacja była w miarę możności czytelna, np.

*Auto> symA 'x'
fromLists [False,True] [False] [True] [(False,'x',[True])]

Wskazówka: przydatne mogą być funkcje

either :: (a -> c) -> (b -> c) -> Either a b -> c
any :: (a -> Bool) -> [a] -> Bool
-}


















