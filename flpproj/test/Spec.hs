import Types
import EquivClassesSetup
import OutputGen ( printEquivClasses, printDFSM )
import InputParser ( loadDFSM )
import MinPrep (ensureTotalTransition, removeNonReachableStates)
import MinDfsmConstructor (createDfsmFromEqClasses)

import Data.List

main :: IO ()
main = do
    srcData <- readFile "test/test4.in"
    --f <- readFile srcData
    let dfsm = ensureTotalTransition $ removeNonReachableStates $ loadDFSM srcData
    putStrLn ""
    --printDFSM dfsm
    --test_inputFormat $ lines f
    --test_IndistinguishabilityRelation dfsm
    test_createDfsmFromEqClasses dfsm (indistinguishabilityRelation dfsm)

test_inputFormat :: [String] -> IO ()
test_inputFormat (x:xs) = do
    putStrLn x
    test_inputFormat xs

test_createDfsmFromEqClasses :: DFSM  -> [[State]] -> IO ()
test_createDfsmFromEqClasses dfsm classes = do
    --printEquivClasses classes
    printDFSM $ createDfsmFromEqClasses dfsm classes

test_IndistinguishabilityRelation :: DFSM -> IO ()
test_IndistinguishabilityRelation dfsm = do
    printEquivClasses $ indistinguishabilityRelation dfsm

test_transTarget :: DFSM -> IO ()
test_transTarget DFSM{d=d} = do
    putStrLn "transTarget test:"
    if transTarget d "4" "b" == "1" then putStrLn "(4->b->1) OK" else putStrLn "(4->b-> FAIL"
    if transTarget d "1" "b" == "2" then putStrLn "(1->b->2) OK" else putStrLn "(1->b-> FAIL"
    if transTarget d "5" "a" == "2" then putStrLn "(5->a->2) OK" else putStrLn "(5->a-> FAIL"
    putStrLn ""

test_isInSameEqClass :: DFSM  -> IO ()
test_isInSameEqClass (DFSM q sigma d q0 f) = do
    
    let testSet = [("1","2"), ("1","3"), ("1","4"), ("1","5"), ("1","6")]
    let classes = [f, q \\f]
    putStrLn "isInSameEqClass test, classes defined as:"
    printEquivClasses classes
    putStrLn ""
    sameClassTest testSet classes sigma d
        where
            sameClassTest :: [(State, Symbol)] ->  [[State]] -> [Symbol] -> [Trans] -> IO ()
            sameClassTest [] _ _ _ = return ()
            sameClassTest ((p,q):xs) classes sigma d = do
                if isInSameEqClass p q classes sigma d then putStrLn (p ++ " " ++ q ++ " - SAME") else putStrLn (p ++ " " ++ q ++ " - DIFFERENT")
                sameClassTest xs classes sigma d

    -- let dfsm = DFSM {
    -- q=["1","2","3","4","5","6"],
    -- sigma=["a", "b"],
    -- d=[Trans "1" "a" "6", Trans "1" "b" "2", Trans "2" "a" "5", 
    --     Trans "2" "b" "4", Trans "3" "a" "3", Trans "3" "b" "6", 
    --     Trans "4" "a" "4", Trans "4" "b" "1", Trans "5" "a" "2", 
    --     Trans "5" "b" "3", Trans "6" "a" "1", Trans "6" "b" "5"],
    -- q0="1",
    -- f=["1","6"]
    -- }