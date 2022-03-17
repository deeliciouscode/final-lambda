module KI.Config where

import KI.Structures

seed :: Int
seed = 420

nBots :: Int 
nBots = 30

botClusterSize :: Int
botClusterSize = 6

playgroundSize :: (Int, Int)
playgroundSize = (960, 540)

playground :: Playground
playground = (take 960 $ repeat 0, take 540 $ repeat 0)
