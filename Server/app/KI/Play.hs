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

import Dungeons.Transform
import Graphics.Image
import Text.Printf
import Types
import KI.Gen

getDungeonDebug :: Int -> IO (PointI, VS.Vector Int, (PointF, Circles), Picture)
getDungeonDebug seed = do
    let imagePath = printf "images/dungeon%d.png" seed
    let meta = ((756.0964,767.90125),[(88.83506,(547.1509,587.6747)),(72.48627,(314.9494,221.06418)),(62.582375,(363.15738,169.11577)),(62.114113,(354.1247,782.55493)),(58.244858,(359.16797,813.0572)),(58.081177,(330.0352,165.16618)),(57.610226,(344.2131,633.53723)),(55.314163,(655.18445,641.47534)),(54.78929,(336.88705,809.55676)),(50.955086,(668.95435,643.7055)),(50.939415,(236.08603,449.1241)),(50.906757,(686.1385,231.98135)),(46.632668,(687.83405,692.10333)),(45.255173,(387.7327,530.61957)),(45.110275,(179.43361,732.58514)),(44.469566,(542.57135,342.3235)),(41.188137,(383.8749,631.3151)),(41.14777,(208.37465,381.1288)),(41.1445,(771.0372,594.44135)),(40.86997,(233.75873,751.0682)),(40.136963,(568.4242,830.293)),(39.142643,(210.4319,297.0638)),(39.031273,(444.33856,494.58423)),(38.350616,(713.8021,616.9759)),(38.225906,(363.00406,838.13007)),(37.08574,(363.42157,574.71796)),(36.84659,(522.4305,751.21216)),(35.170574,(193.56978,719.96014)),(33.632538,(358.8055,805.36914)),(33.472195,(721.2673,778.1685))])
    cluster <- readImageRGB VS imagePath
    let dungeonImage = png $ printf "images/dungeon%d%s.png" seed "Borders"
    (dims, vector) <- transformToVector cluster
    return (dims, vector, meta, dungeonImage)

playWithKI :: IO ()
-- playWithKI (img, dims, flatImage) = play window background fps (makeKIState dims flatImage) render handleKeys moveAgents
playWithKI = do
    (dims, vector, meta, dungeonImage) <- getDungeonDebug 420 
    GAME.play window background fpsTest (initKIDebug 69 (snd meta) vector) (render dungeonImage) handleKeys (moveAgentsDebug vector)

width, height, offset :: Int
width = round sideLen
height = round sideLen
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
                                                pI_position = (700, 700),
                                                pI_direction = (0, 0),
                                                pI_velocity = 5
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
        picture = pictures $ viewDungeon playerMe dungeon : botsToPictures green 5 bots' LST.++ entitiesToPictures red 10 players

viewDungeon :: PlayerInfo -> Picture -> Picture
viewDungeon player dungeon = GAME.translate 0 0 $ GAME.scale mapScale mapScale dungeon

botsToPictures :: Color -> Float -> Entities -> [Picture]
botsToPictures _ _ [] = []
botsToPictures _color size (x:xs) = botPicture : botsToPictures _color size xs
    where
        pos = position x
        dir = direction x
        p = perimeter x
        drawX = fst pos - midDungeon
        drawY = snd pos - midDungeon
        drawPos = (drawX, drawY)
        botPicture = pictures [GLOSS.translate drawX drawY $ color _color $ circleSolid size, color blue $ line [drawPos, tApp2 (+) drawPos $ tApp1 (*p) dir]]
        -- botPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid size -- without the direction lines

entitiesToPictures :: Color -> Float -> [PlayerInfo] -> [Picture]
entitiesToPictures _ _ [] = []
entitiesToPictures _color size (x:xs) = entityPicture : entitiesToPictures _color size xs
    where
        pos = pI_position x
        drawX = fst pos - midDungeon
        drawY = snd pos - midDungeon
        entityPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid size

moveAgentsDebug :: VS.Vector Int -> Float -> KIStateDebug -> KIStateDebug
moveAgentsDebug vector seconds state = state' 
    where 
        playerInfos = view playersDebugL state
        realState = State { 
                KI.Structures.dims = (1000,1000),
                substrate = vector,
                bots = botsD state
              }

        playerInfos' = playersD $ over (playersDebugL . L.traverse . playerMovementL) (movePlayers seconds) state

        realState' = moveAgents playerInfos seconds realState
        state' = StateD { 
                dimsD = (1000, 1000),
                substrateD = vector,
                botsD = bots realState',
                playersD = playerInfos'
              }

movePlayers :: Float -> PlayerMovementAttr -> PlayerMovementAttr
movePlayers seconds ((x,y),(dirX,dirY),v) = ((x+dirX',y+dirY'),(dirX',dirY'),v)
    where
        (dirX',dirY') = normalizeWeighted v (dirX,dirY)