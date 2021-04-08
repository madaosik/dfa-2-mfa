module InputParser
    ( parseArgs, loadDFSM, LaunchMode (Print)
    ) where
import Types ( DFSM(..), State, Symbol, Trans (..))

data LaunchMode = Print | Minimize deriving (Enum, Show, Eq)
type FPath = String

parseArgs :: [String] -> (LaunchMode, Maybe FPath)
parseArgs [] = error "-t launch parametr for DFSM minimization or -i parametr for DFSM print is expected!"
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
loadDFSM inp = extractDFSM $ lines inp

extractDFSM :: [String] -> DFSM
extractDFSM (q:s:[q0]:f:d) = DFSM {q=splitPerComma q, sigma=splitInAlph s, d=getInTrans d, q0=[q0], f=splitPerComma f}
    where
        getInTrans :: [String] -> [Trans]
        getInTrans [] = []
        getInTrans (t:ts) = initInTrans (splitPerComma t) : getInTrans ts

        splitInAlph :: String -> [Symbol]
        splitInAlph [] = []
        splitInAlph (x:xs) = [x] : splitInAlph xs

        splitPerComma :: String -> [String]
        splitPerComma [] = []
        splitPerComma str = x : splitPerComma (drop 1 y) where (x,y) = span (/= ',') str
        --splitPerComma (x:xs)
        --    | x /= ',' = [x] : splitPerComma xs
        --    | otherwise = splitPerComma xs

        initInTrans :: [String] -> Trans
        initInTrans [q0, thru, q1] = Trans {from=q0, thru=thru, to=q1}
