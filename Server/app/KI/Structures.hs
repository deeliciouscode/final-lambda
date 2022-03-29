module KI.Structures where

import Data.Vector.Storable
import Server.LibMessage (PlayerInfo)
import Types
data Entity = Bot  { 
                  stamina :: Int, 
                  style :: String,
                  strength :: Int,
                  awareness :: Int, -- How much is it aware of its surroundings
                  reach :: Int,
                  homebase :: PointF,
                  position :: PointF,
                  direction :: PointF,
                  velocity :: Float,
                  perimeter :: Float,
                  flocking :: Bool
                } 
    deriving (Show)

type Entities = [Entity]

data KIState = State { 
                dims :: PointI,
                substrate :: Vector Int,
                bots :: Entities
              }
    deriving (Show)

data KIStateDebug = StateD { 
                dimsD :: PointI,
                substrateD :: Vector Int,
                botsD :: Entities,
                playersD :: [PlayerInfo]
              }
    deriving (Show)

