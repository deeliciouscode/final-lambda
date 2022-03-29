module KI.Debug where

import KI.KI
import KI.Structures
import KI.Config
import KI.Lenses
import Helpers

import System.Random ()
import Graphics.Gloss as GLOSS
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Data.Function (on)
import Data.List as LST
import qualified Graphics.Image.Interface as IF
import qualified Graphics.Image as GI
import Data.Vector.Unboxed.Base as VUB
import Data.Vector.Storable as VS
import Data.Geometry (vector)
import Graphics.Image.Interface (ColorSpace(getPxC), fromIx, toIx)
import Data.Maybe
import Data.Tuple
import Graphics.Gloss.Export.Image
import Codec.Picture.Png.Internal.Export
import Graphics.Gloss.Data.Vector


-- debugKI :: (Point, [Point]) -> (PointI, VS.Vector (GI.Pixel GI.Y Double)) -> Picture -> IO()
-- debugKI meta map dungeon = do
--     let state = uncurry (makeDebugState meta) map
--     let mvmnt = LST.head $ toListOf (botsL . traverse . movementL) state
--     let wallDistance = calcWallDistanceLRDebug state mvmnt
--     print meta
--     print $ fst map
--     print mvmnt
--     print wallDistance
--     exportPictureToFormat writePng (1000, 1000) black "images/gloss_debug.png" $ renderDebug dungeon state
--     return ()

-- makeDebugState :: (Point, [Point]) -> PointI -> VS.Vector (GI.Pixel GI.Y Double) -> KIState
-- makeDebugState (playerPos, botSpawns) dims vector = State {
--                                         dims = dims,
--                                         substrate = transformToIntVec vector,
--                                         bots = genBotsDebug 1 [LST.head botSpawns]
--                                     }

-- genBotsDebug :: Int -> [PointF] -> Entities
-- genBotsDebug _ [] = []
-- genBotsDebug 0 _ = []
-- genBotsDebug n centers = genBotDebug n centers : genBotsDebug (n-1) centers

-- genBotDebug :: Int -> [PointF] -> Entity
-- genBotDebug i centers = Bot {
--         stamina = stamina',
--         style = style',
--         perimeter = perimeter',
--         strength = strength',
--         velocity = velocity',
--         awareness = awareness',
--         reach = reach',
--         homebase = homebase',
--         position = position',
--         direction = direction',
--         flocking = flocking'
--     }
--     where
--         (cx, cy) = LST.head centers

--         stamina' = 100 :: Int
--         style' = "balanced" :: String
--         perimeter' = 100 :: Float
--         strength' = 10 :: Int
--         awareness' = 10 :: Int
--         reach' = 100 :: Int
--         position' = (cx, cy) :: PointF
--         homebase' = position'
--         direction' = normalize' (1,1) :: PointF
--         velocity' = 50 :: Float
--         -- velocity' = 500 :: Float
--         flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool

-- calcWallDistanceDebug :: KIState -> MovementAttr -> (Float, PointF, PointF, [(Int,Int)], [Int])
-- calcWallDistanceDebug state mvmnt = values
--     where
--         vector = view playgroundL state
--         perimeter = view movementPerimeterL mvmnt
--         cols = fst (view dimsL state)
--         (posX, posY) = view movementPositionL mvmnt
--         c = fromIntegral (fst (view dimsL state)) / 2
--         dx = posX - c 
--         dy = posY - c
--         pos = (c - dy, c + dx)
--         dir@(dirX, dirY) = rotate90CounterClockwise $ view movementDirectionL mvmnt

--         pos's = LST.map (tApp1 round . (tApp2 (+) pos . tApp1Arg (*) (normalize' dir))) [1..perimeter]

--         pixelValues = LST.map ((!) vector . fromIx cols) pos's

--         wallDistance = case LST.findIndex (1 /=) pixelValues of
--             Nothing -> let infinity = read "Infinity"::Float in infinity
--             Just i -> let pos' = pos's !! i in distance pos $ tApp1 fromIntegral pos'

--         values = (wallDistance, dir, pos, LST.take 200 pos's, LST.take 200 pixelValues)

-- calcWallDistanceLRDebug :: KIState -> MovementAttr -> (PointF, PointF, PointF)
-- calcWallDistanceLRDebug state mvmnt = ((wallDistanceLeft, wallDistanceRight), dir, pos)
--     where
--         vector = view playgroundL state
--         perimeter = view movementPerimeterL mvmnt
--         cols = fst (view dimsL state)
--         (posX, posY) = view movementPositionL mvmnt 
--         c = fromIntegral (fst (view dimsL state)) / 2
--         dx = posX - c 
--         dy = posY - c 
--         pos = (c - dy, c + dx) 
        
--         dir@(dirX, dirY) = rotate90CounterClockwise $ view movementDirectionL mvmnt
--         left = rotateV 0.5 dir
--         right = rotateV (-0.5) dir
    
--         pos'sLeft = LST.map (tApp1 round . (tApp2 (+) pos . tApp1Arg (*) (normalize' left))) [1..perimeter]
--         pos'sRight = LST.map (tApp1 round . (tApp2 (+) pos . tApp1Arg (*) (normalize' right))) [1..perimeter]

--         pixelValuesLeft = LST.map ((!) vector . fromIx cols) pos'sLeft
--         pixelValuesRight = LST.map ((!) vector . fromIx cols) pos'sRight

--         wallDistanceLeft = case LST.findIndex (1 /=) pixelValuesLeft of
--             Nothing -> let infinity = read "Infinity"::Float in infinity
--             Just i -> let pos' = pos'sLeft !! i in distance pos $ tApp1 fromIntegral pos'
--         wallDistanceRight = case LST.findIndex (1 /=) pixelValuesRight of
--             Nothing -> let infinity = read "Infinity"::Float in infinity
--             Just i -> let pos' = pos'sRight !! i in distance pos $ tApp1 fromIntegral pos'

-- renderDebug :: Picture -> KIState -> Picture
-- renderDebug dungeon state = picture
--     where
--         bots' = bots state
--         players' = players state
--         playerMe = LST.head players'
--         picture = pictures $ viewDungeon playerMe dungeon : botsToPicturesDebug green 5 bots' LST.++ entitiesToPictures red 10 players'

-- botsToPicturesDebug :: Color -> Float -> Entities -> [Picture]
-- botsToPicturesDebug _ _ [] = []
-- botsToPicturesDebug _color size (x:xs) = botPicture : botsToPicturesDebug _color size xs
--     where
--         pos = position x
--         dir = direction x
--         p = perimeter x
--         drawX = fst pos - fst midPlayground
--         drawY = snd pos - snd midPlayground
--         drawPos = (drawX, drawY)
--         body = GLOSS.translate drawX drawY $ color _color $ circleSolid size
--         left = color blue $ line [drawPos, tApp2 (+) drawPos $ tApp1 (*p) $ rotateV 0.5 dir] 
--         right = color blue $ line [drawPos, tApp2 (+) drawPos $ tApp1 (*p) $ rotateV (-0.5) dir]
--         botPicture = pictures [left, right, body]