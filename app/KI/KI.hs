module KI.KI where

import KI.Structures 
import KI.Config

debugKI :: IO()
debugKI = do
    print playground
    return ()

-- init :: KIState