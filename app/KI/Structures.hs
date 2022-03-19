module KI.Structures where

data Entity = Bot  { 
                  stamina :: Int, 
                  style :: String,
                  perimeter :: Int,
                  strength :: Int,
                  awareness :: Int, -- How much is it aware of its surroundings
                  reach :: Int,
                  position :: (Float, Float),
                  direction :: (Float, Float),
                  velocity :: Float,
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
                substrate :: [[Int]],
                bots :: [Entity],
                players :: [Entity]
              }
    deriving (Show)

--                    position        direction     veloc  perimeter
type MovementAttr = ((Float, Float), (Float, Float), Float, Int)

-- data Playground = 