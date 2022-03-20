module Dungeons.Config where

--------- Randomness --------- 
seed :: Int
seed = 69

--------- Image --------- 
sideLen :: Float
sideLen = 500

midX :: Float
midX = sideLen / 2
midY :: Float
midY = sideLen / 2

--------- Circle Gen --------- 
nCircles :: Int
nCircles = 100
sd :: Float
sd = sideLen / 50
meanRadius :: Float
meanRadius = sideLen / 50

--------- Flocking --------- 
neighborThreshold :: Float
neighborThreshold = sideLen / 6

iterations :: Int
iterations = 30

velocity :: Float
velocity = 1.5
