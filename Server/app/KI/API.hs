{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module KI.API where

import Types
import Server.LibMessage
import KI.Structures
import KI.Config
import Dungeons.Config
import KI.KI
import KI.Gen
import Data.Vector.Storable as VS
import Data.List as LS

initKI :: Int -> [(Float, PointF)] -> VS.Vector Int -> KIState
initKI seed botSpawns vector = State {
                                        dims = (sideLen', sideLen'),
                                        substrate = makeSubstrate 64 sideLen' vector VS.empty,
                                        bots = genBots nBots seed botSpawns
                                    }

makeSubstrate :: Int -> Int -> VS.Vector Int -> VS.Vector Int -> VS.Vector Int
makeSubstrate repeats len vector newVector = vector'
    where 
        (row, rest) = VS.splitAt len vector 
        vector'
            | VS.null rest = newVector VS.++ extrapolate repeats row
            | otherwise =  makeSubstrate repeats len rest $ newVector VS.++ extrapolate repeats row 

extrapolate :: Int -> VS.Vector Int -> VS.Vector Int
-- extrapolate n vector = VS.foldl1 (\vec onTop -> vec VS.++ onTop) $ VS.replicate n $ VS.concatMap (\item -> VS.replicate n item) vector
extrapolate n vector = LS.foldl1 (VS.++) $ LS.replicate n $ VS.concatMap (VS.replicate n) vector

updateKI :: [PlayerInfo] -> Float -> KIState -> KIState
updateKI players seconds state = moveAgents players seconds state

attackKI :: Float -> PlayerInfo -> Int -> KIState -> KIState
attackKI damage player botID state = applyDamage damage player botID state
