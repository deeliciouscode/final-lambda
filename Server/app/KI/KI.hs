{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant section" #-}
module KI.KI where

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


makeKIState :: (Point, [(Float, (Float,Float))]) -> (Int, Int) -> VS.Vector (GI.Pixel GI.Y Double) -> KIState
makeKIState (playerPos, botSpawns) dims vector = State {
                                        dims = dims,
                                        substrate = transformToIntVec vector,
                                        bots = genBots nBots botSpawns,
                                        players = dummyPlayers playerPos
                                    }

transformToIntVec :: VS.Vector (GI.Pixel GI.Y Double) -> VS.Vector Int
transformToIntVec = VS.map (\(GI.PixelY val) -> round val)

-- initialKIState :: KIState
-- initialKIState = State {
--                     dims = (100,100),
--                     substrate = VS.imap (\x -> if (x <= 1000) || (x >= 9000) || (x `mod` 100 < 10) || (x `mod` 100 > 90) then (-)2 else id) (VS.replicate 10000 2),
--                     bots = genBots nBots [(20, 20), (30, 10), (40, 30), (50, 10), (60, 30)],
--                     players = dummyPlayers (50,50)
--                 }

genBots :: Int -> [(Float, (Float,Float))] -> [Entity]
-- genBots = take nBots $ repeat $ genBot $ mkStdGen seed 
genBots _ [] = []
genBots 0 _ = []
genBots n circles = genBot n circles : genBots (n-1) circles

genBot :: Int -> [(Float, (Float,Float))] -> Entity
genBot i circles = Bot {
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
        position' = (cx + position'XDelta, cy + position'YDelta) :: (Float, Float)
        homebase' = position'

        direction' = normalize' (randomNumber (seed - i*1120) (-1) 1, randomNumber (seed - i*11370) (-1) 1) :: (Float, Float)
        velocity' = randomNumber (seed - i*5) 25 30 :: Float
        -- velocity' = 500 :: Float
        flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool

------------------------------- Simulation -------------------------------    

-- simulateKI :: IO ()
-- simulateKI = simulate window background fps initialKIState (render undefined) update

playWithKI :: (Point, [(Float, (Float,Float))]) -> ((Int, Int), VS.Vector (GI.Pixel GI.Y Double)) -> Picture -> IO ()
-- playWithKI (img, dims, flatImage) = play window background fps (makeKIState dims flatImage) render handleKeys moveAgents
playWithKI meta (dims, vector) dungeon = play window background fps (makeKIState meta dims vector) (render dungeon) handleKeys moveAgents

-- maps = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos . (tApp2 (/) `flip`) (fromIntegral n,fromIntegral n)) mvmnt

handleKeys :: Event -> KIState -> KIState
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppSnd  (+(-1))) state
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppSnd  (*0)) state
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppSnd  (+1)) state
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppSnd  (*0)) state
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppFst  (+(-1))) state
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppFst  (*0)) state
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppFst  (+1)) state
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) state = over (playersL . traverse . directionL) (normalizeWeighted 1 . tAppFst  (*0)) state
handleKeys _ state = state

render :: Picture -> KIState -> Picture
render dungeon state = picture
    where
        bots' = bots state
        players' = players state
        playerMe = LST.head players'
        picture = pictures $ viewDungeon playerMe dungeon : botsToPictures green 5 bots' LST.++ entitiesToPictures red 10 players'

viewDungeon :: Entity -> Picture -> Picture
viewDungeon player dungeon = translate 0 0 $ scale mapScale mapScale dungeon

botsToPictures :: Color -> Float -> [Entity] -> [Picture]
botsToPictures _ _ [] = []
botsToPictures _color size (x:xs) = botPicture : botsToPictures _color size xs
    where
        pos = position x
        dir = direction x
        p = perimeter x
        drawX = fst pos - fst midPlayground
        drawY = snd pos - snd midPlayground
        drawPos = (drawX, drawY)
        botPicture = pictures [GLOSS.translate drawX drawY $ color _color $ circleSolid size, color blue $ line [drawPos, tApp2 (+) drawPos $ tApp1 (*p) dir]]
        -- botPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid size -- without the direction lines

entitiesToPictures :: Color -> Float -> [Entity] -> [Picture]
entitiesToPictures _ _ [] = []
entitiesToPictures _color size (x:xs) = entityPicture : entitiesToPictures _color size xs
    where
        pos = position x
        drawX = fst pos - fst midPlayground
        drawY = snd pos - snd midPlayground
        entityPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid size


update :: ViewPort -> Float -> KIState -> KIState
update _ = moveAgents

moveAgents :: Float -> KIState -> KIState
moveAgents seconds state = newState'
    where
        bots = view botsL state
        players = view playersL state
        newState = over (playersL . traverse . playerMovementL) (movePlayers seconds) state
        newBots = over (botsL . traverse . movementL) (moveAgent newState seconds bots players) newState
        newState' = newBots

movePlayers :: Float -> PlayerMovementAttr -> PlayerMovementAttr
movePlayers seconds ((x,y),(dirX,dirY),v) = ((x+dirX',y+dirY'),(dirX',dirY'),v)
    where
        (dirX',dirY') = normalizeWeighted v (dirX,dirY)

moveAgent :: KIState -> Float -> [Entity] -> [Entity] -> MovementAttr -> MovementAttr
moveAgent state seconds bots players movementAttrs = newMovementAttrs'
    where
        newMovementAttrs = calcNewMovementAttrs state seconds bots players $ countNeighbors movementAttrs bots -- 0 is initial no of neighbors
        newMovementAttrs' = updatePosition seconds movementAttrs newMovementAttrs

updatePosition :: Float -> MovementAttr -> MovementAttr -> MovementAttr
updatePosition seconds ((x,y), dir, hb, v, p) (pos, (dirX',dirY'), _, _, _) = ((x'',y''), (dirX',dirY'), hb, v, p)
    where
        x'' = x + dirX' * v * seconds
        y'' = y + dirY' * v * seconds

countNeighbors :: MovementAttr -> [Entity] -> (MovementAttr, Int, [Int])
countNeighbors mvnt@((x,y), (x',y'), hb, v, p) bots = (mvnt, count, indezes)
    where
        positions = LST.map (view positionL) bots
        indezes = LST.map fst (findItems ((>=) p . distance (x, y)) positions)
        count = LST.length indezes

-- first: pretend all have flocking behaviour
calcNewMovementAttrs :: KIState -> Float -> [Entity] -> [Entity] -> (MovementAttr, Int, [Int]) -> MovementAttr
calcNewMovementAttrs state seconds bots players (movementAttrs, n, ixs) = newMovementAttrs
    where
        relevantBots = LST.map (fst . snd) (findItems (flip LST.elem ixs . snd) (zip bots [0..]))

        -- playersPos = toListOf (traverse . positionL) players
        -- closePlayers = filter (tComp (<=)
        --                             (view movementPerimeterL movementAttrs)
        --                             (view movementPositionL movementAttrs)
        --                             . view positionL) players

        nearestPlayerI = fst (LST.minimumBy (compare `on` snd) (zip [0..] (LST.map (distance (view movementPositionL movementAttrs) . view positionL) players)))
        nearestPlayer = players !! nearestPlayerI
        inPerimeter = view movementPerimeterL movementAttrs >= distance (view positionL nearestPlayer) (view movementPositionL movementAttrs)
        inReach = 20 >= distance (view positionL nearestPlayer) (view movementPositionL movementAttrs)
        wallDistanceLR = calcWallDistanceLR state movementAttrs

        (pos, _, hb, v, p) = movementAttrs

        alignment = normalizeA n $ LST.foldr addToMovementsAttrsAlignment movementAttrs relevantBots
        cohesion = normalizeC n $ LST.foldr addToMovementsAttrsCohesion movementAttrs relevantBots
        separation = normalizeS n $ LST.foldr addToMovementsAttrsSeparation movementAttrs relevantBots
        homebaseAwareness = normalizeH $ addToMovementsAttrsHomebaseAwareness movementAttrs
        wallAwareness = normalizeW wallDistanceLR $ addToMovementsAttrsWallAwareness movementAttrs wallDistanceLR
        playerAwareness = normalizeP $ addToMovementsAttrsPlayerAwareness movementAttrs nearestPlayer
        newMovementAttrs
            | inReach = (pos, (0,0), hb, v, p)
            | inPerimeter = normalizeF $ LST.foldr combineSteering movementAttrs [playerAwareness, wallAwareness]
            | otherwise = normalizeF $ LST.foldr combineSteering movementAttrs [alignment, cohesion, separation, homebaseAwareness, wallAwareness]

calcWallDistance :: KIState -> MovementAttr -> Float
calcWallDistance state mvmnt = wallDistance
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

calcWallDistanceLR :: KIState -> MovementAttr -> (Float, Float)
calcWallDistanceLR state mvmnt = (wallDistanceLeft, wallDistanceRight)
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
        left = rotateV 0.5 dir
        right = rotateV (-0.5) dir
    
        pos'sLeft = LST.map (tApp1 round . (tApp2 (+) pos . tApp1Arg (*) (normalize' left))) [1..perimeter]
        pos'sRight = LST.map (tApp1 round . (tApp2 (+) pos . tApp1Arg (*) (normalize' right))) [1..perimeter]

        pixelValuesLeft = LST.map ((!) vector . fromIx cols) pos'sLeft
        pixelValuesRight = LST.map ((!) vector . fromIx cols) pos'sRight

        wallDistanceLeft = case LST.findIndex (1 /=) pixelValuesLeft of
            Nothing -> let infinity = read "Infinity"::Float in infinity
            Just i -> let pos' = pos'sLeft !! i in distance pos $ tApp1 fromIntegral pos'
        wallDistanceRight = case LST.findIndex (1 /=) pixelValuesRight of
            Nothing -> let infinity = read "Infinity"::Float in infinity
            Just i -> let pos' = pos'sRight !! i in distance pos $ tApp1 fromIntegral pos'

addToMovementsAttrsAlignment :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsAlignment bot ((x,y), (dirX,dirY), hb, v, p) = mvmnt'
    where
        (botDirX, botDirY) = view directionL bot
        mvmnt' = ((x,y), (dirX+botDirX,dirY+botDirY), hb, v, p)

addToMovementsAttrsCohesion :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsCohesion bot ((x,y), (dirX,dirY), hb, v, p) = mvmnt'
    where
        (botPosX, botPosY) = view positionL bot
        mvmnt' = ((x,y), (dirX+botPosX,dirY+botPosY), hb, v, p)

addToMovementsAttrsSeparation :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsSeparation bot ((x,y), (dirX,dirY), hb, v, p) = mvmnt'
    where
        (botPosX, botPosY) = view positionL bot
        mvmnt' = ((x,y), (dirX+botPosX-x,dirY+botPosY-y), hb, v, p)

addToMovementsAttrsPlayerAwareness :: MovementAttr -> Entity -> MovementAttr
addToMovementsAttrsPlayerAwareness ((x,y), (dirX,dirY), hb, v, p) player = mvmnt'
    where
        (playerX, playerY) = view positionL player
        mvmnt' = ((x,y), (dirX+playerX,dirY+playerY), hb, v, p)

addToMovementsAttrsHomebaseAwareness :: MovementAttr -> MovementAttr
addToMovementsAttrsHomebaseAwareness ((x,y), (dirX,dirY), hb@(hbX,hbY), v, p) = mvmnt'
    where
        mvmnt' = ((x,y), (dirX+hbX,dirY+hbY), hb, v, p)

addToMovementsAttrsWallAwareness :: MovementAttr -> (Float, Float) -> MovementAttr
addToMovementsAttrsWallAwareness (pos, dir, hb, v, p) (wallDL, wallDR) = (pos, dir', hb, v, p) 
    where
        dir' = if wallDL > wallDR then rotateV 0.5 dir else rotateV (-0.5) dir

normalizeA :: Int -> MovementAttr -> MovementAttr
normalizeA n = over movementDirectionL $ normalizeWeighted 1

normalizeC :: Int -> MovementAttr -> MovementAttr
-- normalizeC n mvmnt = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos . tApp2 (/) (fromIntegral n,fromIntegral n)) mvmnt
normalizeC n mvmnt = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos . (tApp2 (/) `flip`) (fromIntegral n,fromIntegral n)) mvmnt
    where
        pos = view movementPositionL mvmnt

normalizeS :: Int -> MovementAttr -> MovementAttr
normalizeS n = over movementDirectionL (normalizeWeighted 1.5 . tApp1 (*(-1)))

normalizeP :: MovementAttr -> MovementAttr
normalizeP mvmnt = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos) mvmnt
    where
        pos = view movementPositionL mvmnt

normalizeH :: MovementAttr -> MovementAttr
-- normalizeC n mvmnt = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos . tApp2 (/) (fromIntegral n,fromIntegral n)) mvmnt
normalizeH mvmnt = over movementDirectionL (normalizeWeighted weight . (tApp2 (-) `flip`) pos) mvmnt
    where
        pos = view movementPositionL mvmnt
        hb = view movementHomebaseL mvmnt
        weight = distance pos hb / 300

normalizeW :: (Float, Float) -> MovementAttr -> MovementAttr
normalizeW wallDistLR = over movementDirectionL (normalizeWeighted weight)
    where
        wallDist = uncurry min wallDistLR
        weight = if wallDist == (read "Infinity"::Float) then 0 else 300 / (wallDist+eps)**2

normalizeF :: MovementAttr -> MovementAttr
normalizeF = over movementDirectionL normalize'

combineSteering :: MovementAttr -> MovementAttr -> MovementAttr
combineSteering ((x,y), (dirX,dirY), hb, v, p) ((_,_), (dirX',dirY'), _, _, _) = ((x,y), (dirX+dirX',dirY+dirY'), hb, v, p)
