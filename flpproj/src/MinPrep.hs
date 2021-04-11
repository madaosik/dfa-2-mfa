{- 
author: Adam Lanicek
login: xlanic04
year: 2020/2021

Module implementing the preparation steps for the minimization of the DFSM. It provides implementation
to remove non-reachable states as well as functions to ensure that the DFSM passed to the minimization
processing has a totally defined transition function.
-}
module MinPrep (
    removeNonReachableStates, ensureTotalTransition
) where

import Types
import Data.List
import Data.Maybe

-- ------------------------------------------------------------------------------------------------
-- FUNCTIONS FOR REMOVAL OF NONREACHABLE STATES

-- Removes non-reachable states from the given DFSM
removeNonReachableStates :: DFSM -> DFSM
removeNonReachableStates (DFSM q sigma d q0 f) = DFSM q_lean sigma d_lean q0 f_lean
    where
        q_lean = reachableStates q d sigma [q0]
        d_lean = removeIrrelevantTrans d q_lean
        f_lean = removeNonReachableFinal f q_lean

-- Cleans up the set of final states by removing the non-reachable states
removeNonReachableFinal :: [State] -> [State] -> [State]
removeNonReachableFinal [] _ = []
removeNonReachableFinal (qf:qfs) qs
    | qf `elem` qs = qf : removeNonReachableFinal qfs qs
    | otherwise = removeNonReachableFinal qfs qs

-- Creates a set of all reachable states based on the set of all available states, set of 
-- transitions and input alphabet
reachableStates::[State] -> [Trans] -> [Symbol] -> [State] -> [State]
reachableStates q d sigma q_r
    | q_incr == q_r = q_r
    | otherwise = reachableStates q d sigma q_incr
        where
            q_incr = reachableStatesIncrement q d sigma q_r

-- Extends the set of reachable states with the states reachable in the i+1 iteration
reachableStatesIncrement::[State] -> [Trans] -> [Symbol] -> [State] -> [State]
reachableStatesIncrement q d sigma q_r = sort $ nub $ q_r ++ 
                                        [state | state <- q, reachableState state q d sigma]

-- Determines whether a given state is reachable via any of the alphabet symbol from the 
-- current set of reachable states
reachableState:: State -> [State] -> [Trans] -> [Symbol] -> Bool
reachableState q_dest q d = any (tryTransition q_dest q d)

-- Determines whether a given state is reachable via an specific alphabet symbol from the 
-- current set of reachable states
tryTransition :: State -> [State] -> [Trans] -> Symbol -> Bool
tryTransition q_dest q d symb = any (transExists q_dest symb d) q

-- Determines whether there is a transition (from, via, to) in the set of transitions
transExists :: State -> Symbol -> [Trans] -> State -> Bool
transExists q_dest symb d q_from = any (validTrans q_from symb q_dest) d

-- Determines if the combination of starting state & alphabet symbol & destination 
-- state occurs in the set of transitions
validTrans :: State -> Symbol -> State -> Trans -> Bool
validTrans q_from symb q_dest (Trans from thru to) = q_from == from && symb == thru && q_dest == to

---------------------------------------------------------------------------------------------------
-- Removes irrelevant input transitions (the starting state is not in the list of states)
removeIrrelevantTrans :: [Trans] -> [State] -> [Trans]
removeIrrelevantTrans d q= [t | t <- d, stateExists t q]
    where
        stateExists :: Trans -> [State] -> Bool
        stateExists Trans{from=from} q = from `elem` q

-- ------------------------------------------------------------------------------------------------
-- Functions ensuring the DFSM is passed further as a DFSM with total transition function

-- Ensures the provided DFSM has a total transition function. If this is not the case it adds
-- the "SINK" state to the list of states and updates the set of transitions with transitions
-- into the "SINK" state, as well as transitions from the "SINK" state back into the "SINK" state
-- for all the alphabet symbols.
ensureTotalTransition :: DFSM -> DFSM
ensureTotalTransition (DFSM q sigma d q0 f) 
    | isSinkPresent dTotal = DFSM (q ++ ["SINK"]) sigma (addSinkTransitions dTotal sigma) q0 f
    | otherwise = DFSM q sigma dTotal q0 f
    where
        dTotal = ensureTotalTrans q sigma d

-- Boolean to return True if the "SINK" state is present in the list of transitions.
isSinkPresent :: [Trans] -> Bool
isSinkPresent = any isSink
    where
        isSink :: Trans -> Bool
        isSink Trans{to=to}
            | to == "SINK" = True
            | otherwise = False

-- Adds "SINK" to "SINK" transitions for each of the alphabet symbols
addSinkTransitions :: [Trans] -> [Symbol] -> [Trans]
addSinkTransitions d sigma = d ++ map addSinkToSink sigma
    where
        addSinkToSink s = Trans "SINK" s "SINK"

-- -----------------------------------------------------------------------------------------------------
-- Functions ensuring the transition function is defined totally

ensureTotalTrans :: [State] -> [Symbol] -> [Trans] -> [Trans]
ensureTotalTrans [] _ _ = []
ensureTotalTrans (q:qs) sigma d = completeForState q sigma d  ++ ensureTotalTrans qs sigma d

completeForState :: State -> [Symbol] -> [Trans] -> [Trans]
completeForState _ [] _ = []
completeForState q (s:ss) d = completeForStateSymbol q s d ++ completeForState q ss d

completeForStateSymbol :: State -> Symbol -> [Trans] -> [Trans]
completeForStateSymbol q s d = fromMaybe [Trans q s "SINK"] (transFromThruExists q s d)

transFromThruExists :: State -> Symbol -> [Trans] -> Maybe [Trans]
transFromThruExists _ _ [] = Nothing
transFromThruExists q s (Trans from thru to:ds)
    | q == from && s == thru = Just [Trans from thru to]
    | otherwise = transFromThruExists q s ds


