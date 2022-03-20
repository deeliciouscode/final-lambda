module Helpers where

import System.Random

fstOf4 :: (a,b,c,d) -> a
fstOf4 (a, _, _, _) = a

sndOf4 :: (a,b,c,d) -> b
sndOf4 (_, b, _, _) = b

trdOf4 :: (a,b,c,d) -> c
trdOf4 (_, _, c, _) = c

frtOf4 :: (a,b,c,d) -> d
frtOf4 (_, _, _, d) = d

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

findItems :: (a -> Bool) -> [a] -> [(Int, a)]
findItems predicate = filter (predicate . snd) . zip [0..]

round' :: Float -> Integer -> Float
round' num sg = (fromIntegral . round $ num * f) / f
    where f = 10^sg

normalize' :: (Float, Float) -> (Float, Float)
normalize' (vx,vy) = (vx', vy')
                where
                    lenV = distance (0,0) (vx, vy)
                    vx' = round' (vx / lenV) 2
                    vy' = round' (vy / lenV) 2

normalizeWeighted :: Float -> (Float, Float) -> (Float, Float)
normalizeWeighted w (vx,vy) = (vx', vy')
                where
                    lenV = distance (0,0) (vx, vy)
                    vx' = round' ((vx / lenV) * w) 2
                    vy' = round' ((vy / lenV) * w) 2

tApp1 :: (a -> a) -> (a, a) -> (a, a)
tApp1 f (a,b) = (f a, f b)

tApp2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
tApp2 f (a,b) (a',b') = (f a a', f b b')

tComp :: (a -> a -> Bool) -> Float -> (a, a) -> (a, a) -> Bool
tComp f thresh (x,y) (x',y') = True

randomNumber :: (Random a) => Int -> a -> a -> a
randomNumber seed min max = fst $ randomR (min,max) (mkStdGen seed)