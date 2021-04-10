module OutputGen (
    printDFSM, minimizeDFSM, printEquivClasses
) where

import Types
import Data.List

import MinPrep ( removeNonReachableStates, ensureTotalTransition )
import EquivClassesSetup ( indistinguishabilityRelation )
import MinDfsmConstructor ( createDfsmFromEqClasses )

minimizeDFSM :: DFSM -> DFSM
minimizeDFSM dfsm = createDfsmFromEqClasses cleaned_dfsm eqClasses
    where
        cleaned_dfsm = ensureTotalTransition $ removeNonReachableStates dfsm
        eqClasses = indistinguishabilityRelation cleaned_dfsm

printDFSM :: DFSM -> IO ()
printDFSM (DFSM q s d q0 f) = do
    putStrLn $ printStatesCommaSep q
    putStrLn $ concat s
    putStrLn q0
    putStrLn $ printStatesCommaSep f
    printTransitions d

printTransitions :: [Trans] -> IO ()
printTransitions [] = return ()
printTransitions [t] = printTransition t
printTransitions (t:ts) = do
    printTransition t
    printTransitions ts


printTransition :: Trans -> IO ()
printTransition (Trans from thru to) = putStrLn (from ++ "," ++ thru ++ "," ++ to)

printStatesCommaSep :: [[Char]] -> [Char]
printStatesCommaSep = intercalate ","

printEquivClasses :: [[State]] -> IO ()
printEquivClasses [cls] = putStrLn $ printStatesCommaSep cls
printEquivClasses (cls:clss) = do 
    putStrLn $ printStatesCommaSep cls
    printEquivClasses clss
