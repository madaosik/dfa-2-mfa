{- 
author: Adam Lanicek
login: xlanic04
year: 2020/2021

Module implementing steps 8-11 of the Algorithm 3.5 presented in the TIN course material.
It processes the created equivalence classes and creates a new, minimized version of the DFSM. 
-}

module MinDfsmConstructor (
    createDfsmFromEqClasses
) where

import Types
import Data.Char
import Data.List
import EquivClassesSetup ( targetClass )

createDfsmFromEqClasses :: DFSM -> [[State]] -> DFSM
--createDfsmFromEqClasses dfsm _ = dfsm
createDfsmFromEqClasses (DFSM q_old sigma d_old q0_old f_old) classes = DFSM{
        q=newStateNames classes,
        sigma=sigma,
        d=sort $ nub $ newTransitions d_old classes,
        q0=getEqClassIndex (targetClass classes q0_old) classes,
        f=nub $ newFinalStates f_old classes
        }

newStateNames :: [[State]] -> [State]
newStateNames classes = newStates (map concat classes)
    where
        newStates qs_temp = [ [intToDigit x] | x <- [0..(length qs_temp - 1)]]

getEqClassIndex :: [State] -> [[State]] -> State
getEqClassIndex cls classes = case elemIndex cls classes of
    Nothing -> error "Internal error - a class could not be found in the set of generated equivalence classes"
    Just index -> [intToDigit index]

newFinalStates :: [State] -> [[State]] -> [State]
newFinalStates [] _ = []
newFinalStates (qf_old:qf_xs) classes = getEqClassIndex (targetClass classes qf_old) classes : newFinalStates qf_xs classes

newTransitions :: [Trans] -> [[State]] -> [Trans]
newTransitions d classes = map (updateTransBasedonEqClass classes) d
    where 
        updateTransBasedonEqClass :: [[State]] -> Trans -> Trans
        updateTransBasedonEqClass classes (Trans from thru to) = Trans from_n thru to_n
            where
                from_n = getEqClassIndex (targetClass classes from) classes
                to_n = getEqClassIndex (targetClass classes to) classes