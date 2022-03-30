{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant section" #-}
module KI.Play where

import KI.Structures
import KI.Config
import Dungeons.Config
import KI.Lenses
import Helpers

import System.Random ()
import Graphics.Gloss as GLOSS
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game as GAME
import Graphics.Gloss.Game
import Control.Lens as L
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
import Server.LibMessage
import KI.KI
import KI.API

import Dungeons.Transform
import Graphics.Image
import Text.Printf
import Types
import KI.Gen

getDungeonDebug :: Int -> IO (PointI, VS.Vector Int, (PointF, Circles), Picture)
getDungeonDebug seed = do
    let imagePath = printf "images/dungeon%d.png" seed
    let meta = ((78.18364,77.20561),[(10.216839,(57.344578,57.268982)),(8.58196,(29.381441,23.427925)),(7.591571,(35.30174,17.171076)),(7.5447445,(36.936462,78.31102)),(7.157819,(37.773796,81.74373)),(7.141451,(30.39351,18.04513)),(7.0943556,(36.13431,61.621212)),(6.8647494,(64.796936,62.209522)),(6.8122625,(34.97121,81.01571)),(6.4288416,(66.73792,62.57355)),(6.4272747,(24.454586,46.893917)),(6.4240093,(64.745346,28.47063)),(5.9966,(73.014915,62.58034)),(5.8588505,(40.094765,55.86543)),(5.8443604,(20.718353,75.2235)),(5.78029,(53.38563,34.96134)),(5.452147,(39.021988,63.27402)),(5.4481106,(20.56447,38.553875)),(5.4477835,(83.2612,60.783627)),(5.42033,(25.573368,76.75829)),(5.3470297,(57.79042,79.62883)),(5.2475977,(20.746197,30.877884)),(5.2364607,(45.01134,50.45742)),(5.1683946,(73.844696,59.5181)),(5.155924,(38.776905,84.0155)),(5.0419073,(37.209156,57.40132)),(5.017992,(54.460045,73.75773)),(4.8503904,(21.396984,73.84551)),(4.6965866,(37.712048,81.0484)),(4.680553,(71.08122,74.645874))])
    cluster <- readImageRGB VS imagePath
    let dungeonImage = png $ printf "images/dungeon%d.png" seed
    (dims, vector) <- transformToVector cluster
    return (dims, vector, meta, dungeonImage)

playWithKI :: IO ()
-- playWithKI (img, dims, flatImage) = play window background fps (makeKIState dims flatImage) render handleKeys moveAgents
playWithKI = do
    (dims, vector, meta, dungeonImage) <- getDungeonDebug 420
    let vector' = makeSubstrate 64 sideLen' vector VS.empty
    GAME.play window background fpsTest (initKIDebug 69 (snd meta) vector') (render dungeonImage) handleKeys (moveAgentsDebug vector')

width, height, offset :: Int
width = 1000
height = 1000
offset = 0

window :: Display
window = InWindow "Dungeon" (width, height) (offset, offset)

background :: Color
background = black

initKIDebug:: Int -> [(Float, PointF)] -> VS.Vector Int -> KIStateDebug
initKIDebug seed botSpawns vector = StateD {
                                        dimsD = (sideLen', sideLen'),
                                        substrateD = vector,
                                        botsD = genBots nBots seed botSpawns,
                                        playersD = [PI {
                                                pI_mapID = 420, -- -1 als not set
                                                pI_health = (-1, -1), -- (-1,-1) als not set
                                                pI_position = (50 * mapScale, 50 * mapScale),
                                                pI_direction = (0, 0),
                                                pI_velocity = 5/10 * mapScale
                                            }]
                                    }

handleKeys :: Event -> KIStateDebug -> KIStateDebug
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppSnd  (+(-1))) state
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppSnd  (*0)) state
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppSnd  (+1)) state
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppSnd  (*0)) state
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppFst  (+(-1))) state
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppFst  (*0)) state
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppFst  (+1)) state
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) state = over (playersDebugL . L.traverse . directionDebugL) (normalizeWeighted 1 . tAppFst  (*0)) state
handleKeys _ state = state

render :: Picture -> KIStateDebug -> Picture
render dungeon state = picture
    where
        players = view playersDebugL state
        bots' = botsD state
        playerMe = LST.head players
        picture = pictures $ viewDungeon playerMe dungeon : botsToPictures green 5 bots' LST.++ entitiesToPictures red 10 players LST.++ [renderPosPl (LST.head players)]

renderPos :: Entity -> Picture
renderPos bot = GAME.translate (-500) 0 $ color red $ text $ show $ view positionL bot 

renderPosPl :: PlayerInfo -> Picture
renderPosPl player = GAME.translate (-500) 0 $ color red $ text $ show $ view playerPositionL player 

viewDungeon :: PlayerInfo -> Picture -> Picture
-- viewDungeon player dungeon = GAME.translate 0 0 $ GAME.scale mapScale mapScale dungeon
viewDungeon player dungeon = GAME.translate 0 0 $ GAME.scale 10 10 dungeon

botsToPictures :: Color -> Float -> Entities -> [Picture]
botsToPictures _ _ [] = []
botsToPictures _color size (x:xs) = botPicture : botsToPictures _color size xs
    where
        pos = position x
        dir = direction x
        p = perimeter x / 10
        drawX = ((fst pos / mapScale) - midDungeon) * 10
        drawY = ((snd pos / mapScale) - midDungeon) * 10
        drawPos = (drawX, drawY)
        botPicture = pictures [GLOSS.translate drawX drawY $ color _color $ circleSolid size, color blue $ line [drawPos, tApp2 (+) drawPos $ tApp1 (*p) dir]]
        -- botPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid size -- without the direction lines

entitiesToPictures :: Color -> Float -> [PlayerInfo] -> [Picture]
entitiesToPictures _ _ [] = []
entitiesToPictures _color size (x:xs) = entityPicture : entitiesToPictures _color size xs
    where
        pos = pI_position x
        drawX = ((fst pos / mapScale) - midDungeon) * 10
        drawY = ((snd pos / mapScale) - midDungeon) * 10
        entityPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid size

moveAgentsDebug :: VS.Vector Int -> Float -> KIStateDebug -> KIStateDebug
moveAgentsDebug vector seconds state = state'
    where
        playerInfos = view playersDebugL state
        realState = State {
                KI.Structures.dims = (sideLen',sideLen'),
                substrate = vector,
                bots = botsD state
              }

        playerInfos' = playersD $ over (playersDebugL . L.traverse . playerMovementL) (movePlayers seconds) state

        realState' = moveAgents playerInfos seconds realState
        state' = StateD {
                dimsD = (sideLen', sideLen'),
                substrateD = vector,
                botsD = bots realState',
                playersD = playerInfos'
              }

movePlayers :: Float -> PlayerMovementAttr -> PlayerMovementAttr
movePlayers seconds ((x,y),(dirX,dirY),v) = ((x+dirX',y+dirY'),(dirX',dirY'),v)
    where
        (dirX',dirY') = normalizeWeighted v (dirX,dirY)