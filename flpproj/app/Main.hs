module Main where

import InputParser ( parseArgs, LaunchMode (Print), loadDFSM )
import DFSMProcessing ( printDFSM, minimizeDFSM)
import System.IO
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    let (mode, file) = parseArgs args
    srcData <- maybe getContents readFile file
    if mode == Print
        then printDFSM $ loadDFSM srcData 
        else printDFSM $ minimizeDFSM $ loadDFSM srcData 
    return ()



{- showDemo :: Num t1 => (t2 -> t3) -> (t1 -> t2) -> t1 -> t3
showDemo a b c = a $ b (c * c) -}

