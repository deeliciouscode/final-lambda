{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module KI.API where

import Server.LibMessage
import KI.Structures
import KI.Config
import Dungeons.Config
import KI.KI
import KI.Gen
import Data.Vector.Storable as VS

initKI :: Int -> [(Float, (Float,Float))] -> VS.Vector Int -> KIState
initKI seed botSpawns vector = State {
                                        dims = (sideLen', sideLen'),
                                        substrate = vector,
                                        bots = genBots nBots seed botSpawns
                                    }

updateKI :: [PlayerInfo] -> Float -> KIState -> KIState
updateKI players seconds state = moveAgents players seconds state
