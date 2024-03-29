{- 
author: Adam Lanicek
login: xlanic04
year: 2020/2021

Module implementing the parsing of input file and converting the input into the internal DFSM representation
-}

module InputParser
    ( parseArgs, loadDFSM, LaunchMode (Print)
    ) where
import Types ( DFSM(..), State, Symbol, Trans (..))

import Data.List
import Data.Char

data LaunchMode = Print | Minimize deriving (Enum, Show, Eq)
type FPath = String

parseArgs :: [String] -> (LaunchMode, Maybe FPath)
parseArgs [] = error "-t launch parameter for DFSM minimization or -i parametr for DFSM print is expected!"
parseArgs [mode]
    | mode == "-i" = (Print, Nothing)
    | mode == "-t" = (Minimize, Nothing)
    | otherwise = error "Unexpected launch mode has been required! Supported options are -t and -i!"
parseArgs [mode,path]
    | mode == "-i" = (Print, Just path)
    | mode == "-t" = (Minimize, Just path)
    | otherwise = error "Unexpected launch mode has been required! Supported options are -t and -i!"
parseArgs (_:_:_:zs) = error "Too many launch arguments were provided!"

loadDFSM :: String -> DFSM
loadDFSM inp = extractDFSM $ map (filter (/= '\r')) $ lines inp

extractDFSM :: [String] -> DFSM
extractDFSM (q_in:s:[q0]:f:d) = DFSM {
    q = sort q_parsed,
    sigma = sort s_parsed,
    d = sort $ checkForDuplTrans $ map (initInTrans "Transition" q_parsed s_parsed . splitPerComma) $ filter (not . null) d,
    q0 = checkStateMembership "Starting state" q_parsed [q0],
    f = map (checkStateMembership "Final states" q_parsed ) $ sort $ splitPerComma f
    }
    where
        q_parsed = map checkInputStatesFormat $ splitPerComma q_in
        s_parsed = splitInAlph $ checkAlphNonEmptiness s

extractDFSM _ = error "Input file is in an unexpected format!"

checkAlphNonEmptiness :: String -> String
checkAlphNonEmptiness s = if null s then error "Empty input alphabet is not allowed!" else s

checkInputStatesFormat :: State -> State
checkInputStatesFormat [q]
    | isNumber q = [q]
checkInputStatesFormat (q:qs)
    | isNumber q = q : checkInputStatesFormat qs
    | otherwise = error ("States input error: unallowed character '" ++ [q] ++ "' in states definition!")

checkForDuplTrans :: [Trans] -> [Trans]
checkForDuplTrans d
    | length (nub d) == length d = d
    | otherwise = error "Duplicate input transitions detected, terminating!"


checkStateMembership :: String -> [State] -> State -> State
checkStateMembership err_msg qs q
    | q `elem` qs = q
    | otherwise = error (err_msg ++ " input error: '" ++ q ++ "' is not a state from the set " ++ intercalate "-" qs ++ "!")

checkSymbolMembership :: String -> [Symbol] -> State -> State
checkSymbolMembership err_msg ss s
    | s `elem` ss = s
    | otherwise = error (err_msg ++ " input error: '" ++ s ++ "' is not a symbol from the alphabet " ++ intercalate "-" ss ++ "!")

splitInAlph :: String -> [Symbol]
splitInAlph [] = []
splitInAlph (s:ss) = checkAlphSymbol s : splitInAlph ss
    where
        checkAlphSymbol :: Char -> Symbol
        checkAlphSymbol s
            | isLower s = [s]
            | otherwise = error ("Alphabet input error: '" ++ [s] ++ "' is not a lowercase letter!")


splitPerComma :: String -> [String]
splitPerComma [] = []
splitPerComma str = x : splitPerComma (drop 1 y) where (x,y) = span (/= ',') str

initInTrans :: String -> [State] -> [Symbol] -> [String] -> Trans
initInTrans err_msg q s [q_from, thru, q_to] = Trans {
                    from=checkStateMembership err_msg q q_from,
                    thru=checkSymbolMembership err_msg s thru,
                    to=checkStateMembership err_msg q q_to}
initInTrans _ _ _ err_trans = error ("Unexpected format of the input transition: " ++ unwords err_trans)
