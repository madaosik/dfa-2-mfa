-- Module representing the Algorithm 3.4 from the TIN course material for the removal of the nonreachable states

module MinPrep (
    removeNonReachableStates, verifyTotalTransition
) where

import Types
import Data.List
import Data.Maybe

-- --------------------------------------------
-- FUNCTIONS FOR REMOVAL OF NONREACHABLE STATES

removeNonReachableStates :: DFSM -> DFSM
removeNonReachableStates (DFSM q sigma d q0 f) = DFSM {q=q_lean, sigma=sigma, d=d_lean, q0=q0, f=f}
    where
        q_lean = reachableStates q d sigma [q0]
        d_lean = removeIrrelevantTrans d q_lean


-- Creates a set of all reachable states based on the set of all available states, set of transitions and input alphabet
reachableStates::[State] -> [Trans] -> [Symbol] -> [State] -> [State]
reachableStates q d sigma q_r
    | q_incr == q_r = q_r
    | otherwise = reachableStates q d sigma q_incr
        where
            q_incr = reachableStatesIncrement q d sigma q_r


-- Extends the set of reachable states with the states reachable in the i+1 iteration
reachableStatesIncrement::[State] -> [Trans] -> [Symbol] -> [State] -> [State]
reachableStatesIncrement q d sigma q_r = sort $ nub $ q_r ++ [state | state <- q, reachableState state q d sigma]

-- Determines whether a given state is reachable via any of the alphabet symbol from the current set of reachable states
reachableState:: State -> [State] -> [Trans] -> [Symbol] -> Bool
reachableState q_dest q d = any (tryTransition q_dest q d)

-- Determines whether a given state is reachable via an specific alphabet symbol from the current set of reachable states
tryTransition :: State -> [State] -> [Trans] -> Symbol -> Bool
tryTransition q_dest q d symb = any (transExists q_dest symb d) q

-- Determines whether there is a transition (from, via, to) in the set of transitions
transExists :: State -> Symbol -> [Trans] -> State -> Bool
transExists q_dest symb d q_from = any (validTrans q_from symb q_dest) d

-- Determines if the combination of starting state & alphabet symbol & destination state occurs in the set of transitions
validTrans :: State -> Symbol -> State -> Trans -> Bool
validTrans q_from symb q_dest (Trans from thru to) = q_from == from && symb == thru && q_dest == to


-- ---------------------------------------------------------------
-- Functions responsible for the removal of irrelevant transitions
removeIrrelevantTrans :: [Trans] -> [State] -> [Trans]
removeIrrelevantTrans d q= [t | t <- d, stateExists t q]

stateExists :: Trans -> [State] -> Bool
stateExists Trans{from=from} q = from `elem` q

-- ---------------------------------------------------------------
-- Functions ensuring the DFSM is passed further as a DFSM with total transition function
verifyTotalTransition :: DFSM -> DFSM
verifyTotalTransition (DFSM q sigma d q0 f) = DFSM q_total sigma d_total q0 f
    where
        d_total = ensureTotalTrans q sigma d
        q_total = addSinkState q $ isSinkPresent d_total

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

addSinkState :: [State] -> Bool -> [State]
addSinkState q add_bool
    | not add_bool = q
    | otherwise = q ++ ["SINK"]

isSinkPresent :: [Trans] -> Bool
isSinkPresent = any isSink

isSink :: Trans -> Bool
isSink Trans{to=to}
    | to == "SINK" = True
    | otherwise = False
