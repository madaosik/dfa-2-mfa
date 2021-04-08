module MinDfsmConstructor (
    createDfsmFromEqClasses
) where

import Types

createDfsmFromEqClasses :: DFSM -> [[State]] -> DFSM
createDfsmFromEqClasses dfsm _ = dfsm