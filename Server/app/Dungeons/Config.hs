module Dungeons.Config where

--------- Randomness --------- 
testSeed :: Int
testSeed = 420

--------- Dungeon --------- 
sideLen :: Float
sideLen = 100

sideLen' :: Int
sideLen' = round sideLen

midDungeon :: Float
midDungeon = sideLen / 2

midDungeon' :: Int
midDungeon' = round midDungeon

--------- Circle Gen --------- 
nCircles :: Int
nCircles = 100

sd :: Float
sd = sideLen / 50

meanRadius :: Float
meanRadius = sideLen / 30

--------- Flocking --------- 
neighborThreshold :: Float
neighborThreshold = sideLen / 6

iterations :: Int
-- iterations = 30
iterations = 3

genVelocity :: Float 
genVelocity = 1.5