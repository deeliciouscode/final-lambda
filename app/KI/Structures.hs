module KI.Structures where

data Entity = Bot  { 
                  stamina :: Int, 
                  style :: String,
                  perimeter :: Int,
                  strength :: Int,
                  speed :: Int,
                  awareness :: Int, -- How much is it aware of its surroundings
                  reach :: Int,
                  position :: (Int, Int),
                  direction :: (Int, Int),
                  flocking :: Bool
                } 
                | Player { 
                  stamina :: Int,
                  strength :: Int,
                  speed :: Int,
                  position :: (Int, Int),
                  direction :: (Int, Int)
                }
    deriving (Show)


data KIState = State { 
                substrate :: Playground,
                bots :: [Entity],
                players :: [Entity]
              }
    deriving (Show)

type Playground = ([Int], [Int]) 


-- data Playground = 