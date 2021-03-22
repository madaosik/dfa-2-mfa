module Main where

import InputParser ( parseArgs )
import System.IO
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    let (mode, file) = parseArgs args
    print mode
    print file


{- showDemo :: Num t1 => (t2 -> t3) -> (t1 -> t2) -> t1 -> t3
showDemo a b c = a $ b (c * c) -}

