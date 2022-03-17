module KI.Structures where

data Entity = Bot  { 
                  stamina :: Float
                , style :: String
                , perimeter :: Int
                , strength :: Int
                , speed :: Float
                , awareness :: Int -- How much is it aware of its
                , reach :: Int
                , position :: (Int, Int)
                , direction :: (Int, Int)
                } 
                | Player { 
                  stamina :: Float
                , strength :: Int
                , speed :: Float
                , position :: (Int, Int)
                }


data KIState = State { map :: Playground
                    ,  bots :: [Entity]
                    ,  players :: [Entity]
                    }

type Playground = ([Int], [Int]) 


-- data Playground = 