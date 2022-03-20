module KI.Config where

import KI.Structures
import Graphics.Gloss

seed :: Int
seed = 420

nBots :: Int
nBots = 100

botClusterSize :: Int
botClusterSize = 6

playgroundSize :: (Int, Int)
playgroundSize = (960, 540)

playgroundSize' :: (Float, Float)
playgroundSize' = (960, 540)

midPlayground :: (Float, Float)
midPlayground = (fromIntegral (fst playgroundSize) / 2, fromIntegral (snd playgroundSize) / 2)

playground :: [[Int]]
playground = replicate 960 (replicate 540 0)

fps :: Int
fps = 60

window :: Display
window = InWindow "KI" playgroundSize (0, 0)

background :: Color
background = white

dummyPlayers :: [Entity]
dummyPlayers = [
        Player { 
            stamina = 0,
            strength = 0,
            position = (50, 50),
            direction = (0, 0),
            velocity = 5
        }
    ]