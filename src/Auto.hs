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

we have non deterministic computation
we reject the moment we are in no valid state
we accept when the string was consumed and one of the states in valid
-}

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

-- | accepts aut w mówi czy automat aut akceptuje słowo w
-- | Funkcja accepts powinna działać w czasie liniowym zwn. długość słowa.
-- accepts :: Eq q => Auto a q -> [a] -> Bool
-- -- accepts a0 "" = do
-- accepts a0 (x:xs) = do
--     q0 <- initStates a0
--



-- | emptyA rozpoznaje język pusty
emptyA :: Auto a ()
emptyA = A {
    states = [()],
    initStates = [()],
    isAccepting = const False,
    transition = \_ _ -> []
}


-- | epsA rozpoznaje język złożony ze słowa pustego
epsA :: Auto a ()
epsA = A {
    states = [()],
    initStates = [()],
    isAccepting = const True,
    transition = \_ _ -> []
}


-- | symA c rozpoznaje język {c}
symA :: Eq a => a -> Auto a Bool
symA c = A {
    states = [False, True],
    initStates = [False],
    isAccepting = (== True),
    transition = trans
}
    where
        trans False c = [True]
        trans _ _ = []


-- | leftA aut rozpoznaje ten sam język co aut
leftA :: Auto a q -> Auto a (Either q r)
leftA a0 = A {
    states = map Left $ states a0,
    initStates = map Left $ initStates a0,
    isAccepting = \qq -> case qq of {
        Left q -> isAccepting a0 q;
        Right _ -> False
    },
    transition = \qq a -> case qq of {
        Left q -> map Left $ transition a0 q a;
        Right _ -> []
    }
}

-- | język automatu sumA aut1 aut2 jest sumą języków dla aut1 i aut2
sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA a1 a2 = A {
    states = map Left (states a1) ++ map Right (states a2),
    initStates = map Left (initStates a1) ++ map Right (initStates a2),
    isAccepting = \qq -> case qq of {
        Left q -> isAccepting a1 q;
        Right q -> isAccepting a2 q
    },
    transition = \qq a -> case qq of {
        Left q -> map Left $ transition a1 q a;
        Right q -> map Right $ transition a2 q a
    }
}

{-
as for transition, we can transition from the final state of a1 to
any of the initial states of a2; otherwise normal transitions
-}

-- | język automatu thenA aut1 aut2 jest konkatenacją języków dla aut1 i aut2
thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA a1 a2 = A {
    states = map Left (states a1) ++ map Right (states a2),
    initStates = map Left (initStates a1),
    isAccepting = \qq -> case qq of {
            Left q -> False;
            Right q -> isAccepting a2 q
        },
    transition = \qq a -> case qq of {
        Left q -> map Left (transition a1 q a) ++ (if isAccepting a1 q then map Right (initStates a2) else []);
        Right q -> map Right $ transition a2 q a
    }
}


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


















