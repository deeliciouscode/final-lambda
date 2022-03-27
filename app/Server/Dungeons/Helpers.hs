module Server.Dungeons.Helpers where

import Data.List as LIST
import System.Random

nRandoms :: RandomGen g => Int -> Float -> Float -> g -> [Float]
nRandoms n min max = LIST.take n . unfoldr (Just . uniformR (min, max))

round' :: Float -> Integer -> Float
round' num sg = (fromIntegral . round $ num * f) / f
    where f = 10^sg

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

normalize' :: (Float, Float) -> (Float, Float)
normalize' (vx,vy) = (vx', vy')
                where
                    lenV = distance (0,0) (vx, vy)
                    vx' = round' (vx / lenV) 2
                    vy' = round' (vy / lenV) 2
