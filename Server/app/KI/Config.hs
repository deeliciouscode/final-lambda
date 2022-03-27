module KI.Config where

import KI.Structures
import Graphics.Gloss

seed :: Int
seed = 420

mapScale :: Float
mapScale = 1

effectiveScale :: Float 
effectiveScale = mapScale -- depends on map input size, rn its 500x500 and game view is 1000x1000

nBots :: Int
nBots = 50

botClusterSize :: Int
botClusterSize = 6

playgroundSize :: (Int, Int)
playgroundSize = (1000, 1000)

playgroundSize' :: (Float, Float)
playgroundSize' = (1000, 1000)

midPlayground :: (Float, Float)
midPlayground = (fromIntegral (fst playgroundSize) / 2, fromIntegral (snd playgroundSize) / 2)

playground :: [[Int]]
playground = replicate 1000 (replicate 1000 0)

fps :: Int
fps = 60

window :: Display
window = InWindow "KI" playgroundSize (0, 0)

background :: Color
background = white

dummyPlayers :: Point -> [Entity]
dummyPlayers pos = [
        Player { 
            stamina = 0,
            strength = 0,
            position = pos,
            direction = (0, 0),
            velocity = 2
        }
    ]