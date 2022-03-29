{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant section" #-}
module KI.Gen where

import KI.Structures
import Dungeons.Config
import Helpers
import Types 

import Graphics.Gloss as GLOSS
import Data.List as LST

genBots :: Int -> Int -> [(Float, PointF)] -> Entities
genBots _ _ [] = []
genBots 0 _ _ = []
genBots n seed circles = genBot n seed circles : genBots (n-1) seed circles

genBot :: Int -> Int -> [(Float, PointF)] -> Entity
genBot i seed circles = Bot {
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

        stamina' = randomNumber (seed - i) 20 100 :: Int
        encodedStyle = randomNumber (seed - i*2) 0 2 :: Int
        style'
          | encodedStyle == 0 = "aggresive"
          | encodedStyle == 1 = "devensive"
          | otherwise = "balanced" :: String

        -- perimeter' = randomNumber (seed - i*3) 50 80 :: Float
        perimeter' = 100 :: Float

        strength' = randomNumber (seed - i*4) 3 10 :: Int
        awareness' = randomNumber (seed - i*6) 3 10 :: Int
        -- reach' = randomNumber (seed - i*7) 5 10 :: Int
        reach' = 10 :: Int

        position'XDelta = randomNumber (seed - i*8393) (-spread) spread
        position'YDelta = randomNumber (seed - i*82933) (-spread) spread
        position' = (cx + position'XDelta, cy + position'YDelta) :: PointF
        homebase' = position'

        direction' = normalize' (randomNumber (seed - i*1120) (-1) 1, randomNumber (seed - i*11370) (-1) 1) :: PointF
        velocity' = randomNumber (seed - i*5) 25 30 :: Float
        -- velocity' = 500 :: Float
        flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool
