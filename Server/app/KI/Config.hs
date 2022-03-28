module KI.Config where

import KI.Structures
import Graphics.Gloss
import Dungeons.Config

fpsTest :: Int 
fpsTest = 60

mapScale :: Float
mapScale = 1

effectiveScale :: Float 
effectiveScale = mapScale -- depends on map input size, rn its 500x500 and game view is 1000x1000

nBots :: Int
nBots = 50
