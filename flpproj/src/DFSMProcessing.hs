module DFSMProcessing (
    printDFSM, minimizeDFSM
) where

import Types
import Data.List

minimizeDFSM :: DFSM -> DFSM
minimizeDFSM = id

printDFSM :: DFSM -> IO ()
printDFSM (DFSM q s d q0 f) = do
    putStrLn $ printStatesCommaSep q
    putStrLn $ concat s
    printTransitions d
    putStrLn q0
    putStrLn $ printStatesCommaSep f

--printStates :: [String] -> String
--printStates xs = concat xs

printTransitions :: [Trans] -> IO ()
printTransitions [t] = printTransition t
printTransitions (t:ts) = do
    printTransition t
    printTransitions ts

printTransition (Trans from via to) = putStrLn (from ++ "," ++ via ++ "," ++ to)
printStatesCommaSep = intercalate ","
