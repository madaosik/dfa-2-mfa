module InputParser
    ( parseArgs
    ) where

data LaunchMode = Print | Minimize deriving (Enum, Show)
type FPath = String

parseArgs :: [String] -> (LaunchMode, Maybe FPath)
parseArgs [mode]
    | mode == "-i" = (Print, Nothing)
    | mode == "-t" = (Minimize, Nothing)
    | otherwise = error "Unexpected launch mode has been required! Supported options are -t and -i!"
parseArgs [mode,path]
    | mode == "-i" = (Print, Just path)
    | mode == "-t" = (Minimize, Just path)
    | otherwise = error "Unexpected launch mode has been required! Supported options are -t and -i!"
parseArgs (_:_:_:zs) = error "Too many launch arguments were provided!"
