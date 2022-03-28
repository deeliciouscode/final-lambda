module KI.Structures where

import Data.Vector.Storable
import Server.LibMessage (PlayerInfo)
data Entity = Bot  { 
                  stamina :: Int, 
                  style :: String,
                  strength :: Int,
                  awareness :: Int, -- How much is it aware of its surroundings
                  reach :: Int,
                  homebase :: (Float, Float),
                  position :: (Float, Float),
                  direction :: (Float, Float),
                  velocity :: Float,
                  perimeter :: Float,
                  flocking :: Bool
                } 
    deriving (Show)


data KIState = State { 
                dims :: (Int, Int),
                substrate :: Vector Int,
                bots :: [Entity]
                -- players :: [Entity]
              }
    deriving (Show)

data KIStateDebug = StateD { 
                dimsD :: (Int, Int),
                substrateD :: Vector Int,
                botsD :: [Entity],
                playersD :: [PlayerInfo]
              }
    deriving (Show)

--                    position        direction      homebase        veloc  perimeter
type MovementAttr = ((Float, Float), (Float, Float), (Float, Float), Float, Float)

--                          position        direction      velocity 
type PlayerMovementAttr = ((Float, Float), (Float, Float), Float)

-- data Playground = 