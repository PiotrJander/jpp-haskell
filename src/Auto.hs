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

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
    show = show . toLists


-- newtype Alpha = Alpha Char deriving (Eq, Enum, Show)
--
-- instance Bounded Alpha where
--     minBound = Alpha 'A'
--     maxBound = Alpha 'Z'
--
-- -- instance Enum Alpha where
-- --     show (Alpha c) = show c
-- --
-- -- instance Show Alpha where ...


-- | accepts aut w mówi czy automat aut akceptuje słowo w
-- Funkcja accepts powinna działać w czasie liniowym zwn. długość słowa.
-- TODO how to filter duplicates with the list monad
accepts :: Eq q => Auto a q -> [a] -> Bool
accepts a0 xs = any (isAccepting a0) $ accepts' a0 (initStates a0) xs

accepts' :: Eq q => Auto a q -> [q] -> [a] -> [q]
accepts' _ qs [] = qs
accepts' a0 qs (x:xs) =
    let
--         qs' = nub $ qs >>= \q -> transition a0 q x
        qs' = nub $ do
            q <- qs
            transition a0 q x
    in
        accepts' a0 qs' xs

-- accepts' :: Eq q => Auto a q -> q -> [a] -> [q]
-- accepts' a0 q [] = [ q ]
-- accepts' a0 q (x:xs) = do
--     q' <- transition a0 q x
-- --     accepts' a0 q' xs
--     nub $ accepts' a0 q' xs


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
        trans False ch = [True | ch == c]
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


-- | język automatu thenA aut1 aut2 jest konkatenacją języków dla aut1 i aut2
thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA a1 a2 = A {
    states = map Left (states a1) ++ map Right (states a2),
    initStates = map Left (initStates a1) ++ (if any (isAccepting a1) (initStates a1) then map Right (initStates a2) else []),
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
fromLists states initStates finalStates transitions = A {
    states = states,
    initStates = initStates,
    isAccepting = (`elem` finalStates),
    transition = \q a -> case fun q a of {Just (_, _, dest) -> dest; Nothing -> []}
}
    where
        fun q a = find (\(q', a', _) -> q == q' && a == a') transitions


-- | toLists tłumaczy pomiędzy reprezentacją funkcyjną a reprezentacją listową (stany,startowe,akceptujące,przejścia)
toLists :: (Enum a, Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists a0 = (states a0, initStates a0, filter (isAccepting a0) (states a0), trans)
    where
        trans = [ (q, c, dest) | q <- states a0, c <- [minBound .. ], let dest = transition a0 q c, not $ null dest]


-- tests
--
-- data AB = AA | BB deriving(Eq,Ord,Show,Bounded,Enum)
--
--
--
-- aut0 :: Auto AB Int
-- aut0 = fromLists a b c d
--     where
--         (a, b, c, d) = ([0,1,2],[1],[2],[(0,AA,[2]),(0,BB,[1,2]),(1,AA,[]),(1,BB,[1]),(2,AA,[]),(2,BB,[1,2])])

--
-- ttable = [(0,AA,[2]),(0,BB,[1,2]),(1,AA,[]),(1,BB,[1]),(2,AA,[]),(2,BB,[1,2])]
--
--
-- fun ttable q a = find (\(q', a', _) -> q == q' && a == a') ttable
--
--
--
-- exper = case (0, AA, [2]) of {(1, AA, _) -> True; _ -> False}
--
--


