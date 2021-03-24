module Types
    ( DFSM (..), Trans (..), State, Symbol
    ) where

-- M = (Q, sigma, delta, q0, F)
-- Q...mnozina stavu
-- sigma....abeceda (mnozina znaku)
-- delta....prechodova funkce Qxsigma -> 2^Q
-- qo....pocatecni stav
-- F....mnozina koncovych stavu
data Trans = Trans {
    from::State,
    via::Symbol,
    to::State
} deriving Show

type State = String
type Symbol = String

data DFSM = DFSM {
    q :: [State],
    sigma :: [Symbol],
    d :: [Trans],
    q0 :: State,
    f :: [State]
} deriving Show

