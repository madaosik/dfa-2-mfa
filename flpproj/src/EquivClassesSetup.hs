{- 
author: Adam Lanicek
login: xlanic04
year: 2020/2021

Module implementing the steps 1-7 of the Algorithm 3.5 (equivalence classes setup)
from the TIN course material for the minimization of the DFSM.
-}

module EquivClassesSetup (
    indistinguishabilityRelation,
    transTarget, isInSameEqClass, targetClass
) where

import Types
import Data.List

-- Creates the indistinguishability relation for the given DFSM
indistinguishabilityRelation :: DFSM -> [[State]]
indistinguishabilityRelation (DFSM q sigma d q0 f) = createEquivClasses [f, q \\ f] sigma d

-- Sets up the equivalence classes based on the list of lists of input states
createEquivClasses :: [[State]] -> [Symbol] -> [Trans] -> [[State]]
createEquivClasses classes sigma d
    | classes == higherDegreeClasses = classes
    | otherwise = createEquivClasses higherDegreeClasses sigma d
        where
            higherDegreeClasses = higherDegreeRelation classes sigma d

-- Facilitates the equivalence analysis for each of the equivalence class
higherDegreeRelation :: [[State]] -> [Symbol] -> [Trans] -> [[State]]
higherDegreeRelation clss sigma d = if null disrClasses then sort okClasses else sort (okClasses ++ [disrClasses])
    where
        inspectedClasses = [inspectClass eqClass clss sigma d | eqClass <- clss, length eqClass > 1]
        okClasses = [singletonClass | singletonClass <- clss, length singletonClass == 1] ++ map fst inspectedClasses
        disrClasses = concatMap snd inspectedClasses

-- Inspects the given equivalence class and produces a tuple: (list of states in same eq group, states not in the same group)
-- Example of the input : [1,3,4]
-- Example of the output : ([1,3],[4]) or ([1,4,3],[]) or ([1], [3,4])
inspectClass :: [State] -> [[State]] -> [Symbol] -> [Trans] -> ([State], [State])
inspectClass eqCls cls sigma d = (nonDividedClass, dividedClass)
    where
        nonDividedClass = eqClassRepr : [q_tested | q_tested <- restOfClass, isInSameEqClass eqClassRepr q_tested cls sigma d]
        dividedClass = [q_tested | q_tested <- restOfClass, not (isInSameEqClass eqClassRepr q_tested cls sigma d)]
        eqClassRepr = head eqCls
        restOfClass = tail eqCls

-- Tests if two states are members of the same equivalence class
isInSameEqClass :: State -> State -> [[State]] -> [Symbol] -> [Trans] -> Bool
isInSameEqClass p q cls sigma d = pClasses == qClasses
    where
        pTargets = map (transTarget d p) sigma
        qTargets = map (transTarget d q) sigma
        pClasses = map (targetClass cls) pTargets
        qClasses = map (targetClass cls) qTargets

-- Returns the transition target based on the state and input symbol
transTarget :: [Trans] -> State -> Symbol -> State
transTarget [] q s = error ("INTERNAL ERROR: Undefined transition from the state '" ++ q ++ "' using symbol '" ++ s ++ "'!")
transTarget (Trans from thru to:ds) q s
    | from == q && thru == s = to
    | otherwise = transTarget ds q s

-- Returns the equivalence class of the state provided as an argument
-- input: List of equivalence classes and a state whose equivalence class are we interested in
-- output: Equivalence class of the state
targetClass :: [[State]] -> State -> [State]
targetClass (eqClass:restOfClasses) q
    | q `elem` eqClass = eqClass
    | otherwise = targetClass restOfClasses q