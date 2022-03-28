module KI.Structures where

import Data.Vector.Storable
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
                | Player { 
                  stamina :: Int,
                  strength :: Int,
                  position :: (Float, Float),
                  direction :: (Float, Float),
                  velocity :: Float
                }
    deriving (Show)


data KIState = State { 
                dims :: (Int, Int),
                substrate :: Vector Int,
                bots :: [Entity],
                players :: [Entity]
              }
    deriving (Show)

--                    position        direction      homebase        veloc  perimeter
type MovementAttr = ((Float, Float), (Float, Float), (Float, Float), Float, Float)

--                          position        direction      velocity 
type PlayerMovementAttr = ((Float, Float), (Float, Float), Float)

-- data Playground = 