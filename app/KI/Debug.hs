module KI.Debug where

import KI.KI
import KI.Structures
import KI.Config
import System.Random ()
import KI.Lenses
import Graphics.Gloss as GLOSS
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Helpers
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

debugKI :: (Point, [Point]) -> ((Int, Int), VS.Vector (GI.Pixel GI.Y Double)) -> Picture -> IO()
debugKI meta map dungeon = do
    let state = uncurry (makeDebugState meta) map
    let mvmnt = LST.head $ toListOf (botsL . traverse . movementL) state
    let wallDistance = calcWallDistanceDebug state mvmnt
    print meta
    print $ fst map
    -- print dungeon
    print mvmnt
    print wallDistance
    -- print $ LST.head $ toListOf (botsL . traverse . perimeterL) state
    exportPictureToFormat writePng (1000, 1000) black "images/gloss_debug.png" $ render dungeon state
    return ()


makeDebugState :: (Point, [Point]) -> (Int, Int) -> VS.Vector (GI.Pixel GI.Y Double) -> KIState
makeDebugState (playerPos, botSpawns) dims vector = State {
                                        dims = dims,
                                        substrate = transformToIntVec vector,
                                        bots = genBotsDebug 1 [LST.head botSpawns],
                                        players = dummyPlayers playerPos
                                    }

genBotsDebug :: Int -> [(Float, Float)] -> [Entity]
-- genBots = take nBots $ repeat $ genBot $ mkStdGen seed 
genBotsDebug _ [] = []
genBotsDebug 0 _ = []
genBotsDebug n centers = genBotDebug n centers : genBotsDebug (n-1) centers

genBotDebug :: Int -> [(Float, Float)] -> Entity
genBotDebug i centers = Bot {
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
        (cx, cy) = LST.head centers

        stamina' = 100 :: Int
        style' = "balanced" :: String
        perimeter' = 100 :: Float
        strength' = 10 :: Int
        awareness' = 10 :: Int
        reach' = 100 :: Int
        position' = (cx, cy) :: (Float, Float)
        homebase' = position'
        direction' = normalize' (1,0) :: (Float, Float)
        velocity' = 50 :: Float
        -- velocity' = 500 :: Float
        flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool


calcWallDistanceDebug :: KIState -> MovementAttr -> (Float, (Float,Float), (Float,Float), [(Int,Int)], [Int])
calcWallDistanceDebug state mvmnt = values
    where
        vector = view playgroundL state
        perimeter = view movementPerimeterL mvmnt
        cols = fst (view dimsL state)
        (posX, posY) = view movementPositionL mvmnt -- 900,900
        c = fromIntegral (fst (view dimsL state)) / 2
        dx = posX - c -- 400
        dy = posY - c -- 400
        pos = (c - dy, c + dx) -- (900, 100)
        -- pos = (posX, 999-posY)
        dir@(dirX, dirY) = rotate90CounterClockwise $ view movementDirectionL mvmnt

        pos's = LST.map (tApp1 round . (tApp2 (+) pos . tApp1Arg (*) (normalize' dir))) [1..perimeter]

        pixelValues = LST.map ((!) vector . fromIx cols) pos's

        wallDistance = case LST.findIndex (1 /=) pixelValues of
            Nothing -> let infinity = read "Infinity"::Float in infinity
            Just i -> let pos' = pos's !! i in distance pos $ tApp1 fromIntegral pos'

        values = (wallDistance, dir, pos, LST.take 200 pos's, LST.take 200 pixelValues)
