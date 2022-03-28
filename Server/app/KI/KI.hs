{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant section" #-}
module KI.KI where

import KI.Structures
import Dungeons.Config
import KI.Lenses
import Helpers

import Graphics.Gloss as GLOSS
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Data.Function
import Data.List as LST
import Data.Vector.Unboxed.Base as VUB
import Data.Vector.Storable as VS
import Graphics.Image.Interface (ColorSpace(getPxC), fromIx, toIx)
import Graphics.Gloss.Export.Image
import Codec.Picture.Png.Internal.Export
import Graphics.Gloss.Data.Vector
import Server.LibMessage

moveAgents :: [PlayerInfo] -> Float -> KIState -> KIState
moveAgents players seconds state = newState'
    where
        bots = view botsL state
        newBots = over (botsL . traverse . movementL) (moveAgent state seconds bots players) state
        newState' = newBots

-- movePlayers :: Float -> PlayerMovementAttr -> PlayerMovementAttr
-- movePlayers seconds ((x,y),(dirX,dirY),v) = ((x+dirX',y+dirY'),(dirX',dirY'),v)
--     where
--         (dirX',dirY') = normalizeWeighted v (dirX,dirY)

moveAgent :: KIState -> Float -> [Entity] -> [PlayerInfo] -> MovementAttr -> MovementAttr
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

calcNewMovementAttrs :: KIState -> Float -> [Entity] -> [PlayerInfo] -> (MovementAttr, Int, [Int]) -> MovementAttr
calcNewMovementAttrs state seconds bots players (movementAttrs, n, ixs) = newMovementAttrs
    where
        relevantBots = LST.map (fst . snd) (findItems (flip LST.elem ixs . snd) (zip bots [0..]))
        nearestPlayerI = fst (LST.minimumBy (compare `on` snd) (zip [0..] (LST.map (distance (view movementPositionL movementAttrs) . view playerPositionL) players)))
        nearestPlayer = players !! nearestPlayerI
        inPerimeter = view movementPerimeterL movementAttrs >= distance (view playerPositionL nearestPlayer) (view movementPositionL movementAttrs)
        inReach = 20 >= distance (view playerPositionL nearestPlayer) (view movementPositionL movementAttrs)
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

addToMovementsAttrsPlayerAwareness :: MovementAttr -> PlayerInfo -> MovementAttr
addToMovementsAttrsPlayerAwareness ((x,y), (dirX,dirY), hb, v, p) player = mvmnt'
    where
        (playerX, playerY) = view playerPositionL player
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
