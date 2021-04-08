module Main where

import InputParser ( parseArgs, LaunchMode (Print), loadDFSM )
import OutputGen ( printDFSM, minimizeDFSM, printEquivClasses)
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

