{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant section" #-}
module KI.Gen where

import KI.Structures
import Dungeons.Config
import KI.Config
import Helpers
import Types 
import Data.Vector.Storable as VS
import Graphics.Gloss as GLOSS
import Data.List as LST

genNPCs :: Int -> Entities
genNPCs 0 = [] 
genNPCs n = genNPC n : genNPCs (n-1)

genNPC :: Int -> Entity
genNPC n = NPC n (100*fromIntegral n, 200/fromIntegral n)

genBots :: Int -> Int -> [(Float, PointF)] -> Entities
genBots _ _ [] = []
genBots 0 _ _ = []
genBots n seed circles = genBot n seed circles : genBots (n-1) seed circles

genBot :: Int -> Int -> [(Float, PointF)] -> Entity
genBot i seed circles = Bot {
        botID = 1000 + i,
        stamina = stamina',
        style = style',
        perimeter = perimeter',
        strength = strength',
        velocity = velocity',
        awareness = awareness',
        reach = reach',
        homebase = homebase',
        position = position',
        direction = direction',
        flocking = flocking'
    }
    where
        centers = LST.map snd circles
        radii = LST.map fst circles
        iCenter = randomNumber (seed - i * 23247) 0 (LST.length centers - 1) :: Int
        (cx, cy) = centers !! iCenter
        spread = (radii !! iCenter) / 2
        -- spread = 1

        stamina' = randomNumber (seed - i) 20 100 :: Float
        encodedStyle = randomNumber (seed - i*2) 0 2 :: Int
        style'
          | encodedStyle == 0 = "aggresive"
          | encodedStyle == 1 = "devensive"
          | otherwise = "balanced" :: String

        -- perimeter' = randomNumber (seed - i*3) 50 80 :: Float
        perimeter' = mapScale * 10 :: Float

        strength' = randomNumber (seed - i*4) 3 10 :: Float
        awareness' = randomNumber (seed - i*6) 3 10 :: Float
        -- reach' = randomNumber (seed - i*7) 5 10 :: Int
        reach' = 10 :: Float

        position'XDelta = randomNumber (seed - i*8393) (-spread) spread
        position'YDelta = randomNumber (seed - i*82933) (-spread) spread
        position' = (cx * mapScale + position'XDelta * mapScale, cy * mapScale + position'YDelta * mapScale) :: PointF
        homebase' = position'

        direction' = normalize' (randomNumber (seed - i*1120) (-1) 1, randomNumber (seed - i*11370) (-1) 1) :: PointF
        velocity' = randomNumber (seed - i*5) 75 100 :: Float
        -- velocity' = 500 :: Float
        flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool

makeSubstrate :: Int -> Int -> VS.Vector Int -> VS.Vector Int -> VS.Vector Int
makeSubstrate repeats len vector newVector = vector'
    where
        (row, rest) = VS.splitAt len vector
        vector'
            | VS.null rest = newVector VS.++ extrapolate repeats row
            | otherwise =  makeSubstrate repeats len rest $ newVector VS.++ extrapolate repeats row

extrapolate :: Int -> VS.Vector Int -> VS.Vector Int
-- extrapolate n vector = VS.foldl1 (\vec onTop -> vec VS.++ onTop) $ VS.replicate n $ VS.concatMap (\item -> VS.replicate n item) vector
extrapolate n vector = LST.foldl1 (VS.++) $ LST.replicate n $ VS.concatMap (VS.replicate n) vector
