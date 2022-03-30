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
import Helpers

initKI :: Int -> [(Float, PointF)] -> VS.Vector Int -> KIState
initKI seed botSpawns vector = State {
                                        dims = (sideLen', sideLen'),
                                        substrate = makeSubstrate 64 sideLen' vector VS.empty,
                                        bots = genBots nBots seed botSpawns
                                    }

updateKI :: [PlayerInfo] -> Float -> KIState -> KIState
updateKI players seconds state = moveAgents players seconds state

attackKI :: Float -> PlayerInfo -> Int -> KIState -> KIState
attackKI damage player botID state = applyDamage damage player botID state

initNPCs :: Entities
initNPCs = genNPCs nNPCs

getNpcID :: Entities -> PointF -> Int
getNpcID npcs pos = id
    where
        zipped = zip [0..] (LS.map (distance pos . position)  npcs) :: [(Int, Float)]
        distances = sortBy (\a b -> if snd a >= snd b then GT else LT) zipped
        id = botID $ npcs !! fst (LS.head distances)
