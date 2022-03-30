module KI.Config where

fpsTest :: Int 
fpsTest = 60

mapScale :: Float
mapScale = 64

effectiveScale :: Float 
effectiveScale = mapScale -- depends on map input size, rn its 500x500 and game view is 1000x1000

nBots :: Int
nBots = 50

nNPCs :: Int
nNPCs = 5