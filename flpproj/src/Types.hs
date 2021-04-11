{- 
author: Adam Lanicek
login: xlanic04
year: 2020/2021

Module defining the types used in the DFSM minimization algorithm.
Defines the type representing the finite state machine and its transitions.
-}
module Types
    ( DFSM (..), Trans (..), State, Symbol
    ) where

-- Represent one state
type State = String
-- Represents one alphabet symbol
type Symbol = String

{- 
Data type representing a finite state machine

M = (Q, sigma, delta, q0, F)
Q........set of states
sigma....input alphabet (set of symbols)
delta....transition function Q x sigma -> 2^Q
qo.......starting state, q0 is part of Q
F........set of end states, F is subset of Q 
-}
data DFSM = DFSM {
    q :: [State],
    sigma :: [Symbol],
    d :: [Trans],
    q0 :: State,
    f :: [State]
} deriving Show

{- 
Data type representing a transition of a finite state machine

from....starting set
thru....input symbol from the input alphabet
to......destination set of the transition
-}
data Trans = Trans {
    from::State,
    thru::Symbol,
    to::State
} deriving (Show, Eq, Ord)

