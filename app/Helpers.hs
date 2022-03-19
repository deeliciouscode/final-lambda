module Helpers where

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
