module KI.Structures where

import Data.Vector.Storable
import Server.LibMessage
import Types
data Entity = Bot  { 
                  botID :: Int,
                  stamina :: Float, 
                  strength :: Float,
                  style :: String,
                  awareness :: Float, -- How much is it aware of its surroundings
                  reach :: Float,
                  homebase :: PointF,
                  position :: PointF,
                  direction :: PointF,
                  velocity :: Float,
                  perimeter :: Float,
                  flocking :: Bool
                } | NPC {
                  botID :: Int,
                  position :: PointF
                }
    deriving (Show)

type Entities = [Entity]

data KIState = State { 
                dims :: PointI,
                substrate :: Vector Int,
                bots :: Entities
              }
    deriving (Show)

newtype NPCState = StateNPC {
                npcs :: Entities
              }
data KIStateDebug = StateD { 
                dimsD :: PointI,
                substrateD :: Vector Int,
                botsD :: Entities,
                playersD :: [PlayerInfo]
              }
    deriving (Show)
