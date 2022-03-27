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

fstOf5 :: (a,b,c,d,e) -> a
fstOf5 (a, _, _, _, _) = a

sndOf5 :: (a,b,c,d,e) -> b
sndOf5 (_, b, _, _, _) = b

trdOf5 :: (a,b,c,d,e) -> c
trdOf5 (_, _, c, _, _) = c

frtOf5 :: (a,b,c,d,e) -> d
frtOf5 (_, _, _, d, _) = d

fftOf5 :: (a,b,c,d,e) -> e
fftOf5 (_, _, _, _, e) = e

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

findItems :: (a -> Bool) -> [a] -> [(Int, a)]
findItems predicate = filter (predicate . snd) . zip [0..]

round' :: Float -> Integer -> Float
round' num sg = (fromIntegral . round $ num * f) / f
    where f = 10^sg

normalize' :: (Float, Float) -> (Float, Float)
normalize' (0,0) = (0, 0)
normalize' (vx,vy) = (vx', vy')
                where
                    lenV = distance (0,0) (vx, vy)
                    vx' = round' (vx / lenV) 2
                    vy' = round' (vy / lenV) 2

normalizeWeighted :: Float -> (Float, Float) -> (Float, Float)
normalizeWeighted _ (0,0) = (0, 0)
normalizeWeighted w (vx,vy) = (vx', vy')
                where
                    lenV = distance (0,0) (vx, vy)
                    vx' = round' ((vx / lenV) * w) 2
                    vy' = round' ((vy / lenV) * w) 2

tApp1 :: (a -> b) -> (a, a) -> (b, b)
tApp1 f (a,b) = (f a, f b)

tApp1Arg :: (a -> a -> a) -> (a, a) -> a -> (a, a)
tApp1Arg f (a,b) c = (f a c, f b c)

tAppFst :: (a -> a) -> (a, a) -> (a, a)
tAppFst f (a,b) = (f a, b)

tAppSnd :: (a -> a) -> (a, a) -> (a, a)
tAppSnd f (a,b) = (a, f b)

tApp2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
tApp2 f (a,b) (a',b') = (f a a', f b b')

tComp :: (a -> a -> Bool) -> Float -> (a, a) -> (a, a) -> Bool
tComp f thresh (x,y) (x',y') = True

randomNumber :: (Random a) => Int -> a -> a -> a
randomNumber seed min max = fst $ randomR (min,max) (mkStdGen seed)

eps :: Float 
eps = 0.00000000000001

rotate90Clockwise :: (Float, Float) -> (Float, Float)
rotate90Clockwise (x, y) = (y, x*(-1))

rotate90CounterClockwise :: (Float, Float) -> (Float, Float)
rotate90CounterClockwise (x, y) = (y*(-1), x)

rotate :: Float -> (Float, Float) -> (Float, Float)
rotate beta (x, y) = (cos beta * x - sin beta * y, cos beta * x - sin beta * y)

