module KI.Config where

import KI.Structures
import Graphics.Gloss

seed :: Int
seed = 420

nBots :: Int
nBots = 30

botClusterSize :: Int
botClusterSize = 6

playgroundSize :: (Int, Int)
playgroundSize = (960, 540)

midPlayground :: (Float, Float)
midPlayground = (fromIntegral (fst playgroundSize) / 2, fromIntegral (snd playgroundSize) / 2)

playground :: [[Int]]
playground = replicate 960 (replicate 540 0)

fps :: Int
fps = 30

window :: Display
window = InWindow "KI" playgroundSize (0, 0)

background :: Color
background = white