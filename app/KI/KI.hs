{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant section" #-}
module KI.KI where

import KI.Structures
import KI.Config
import System.Random
import KI.Lenses
import Graphics.Gloss as GLOSS
import Graphics.Gloss.Data.ViewPort
import Control.Lens
import Helpers
import Data.Function (on)
import Data.List

debugKI :: IO()
debugKI = do
    print playground
    print $ genBots nBots [(30, 30)]
    return ()

initialKIState :: KIState
initialKIState = State {
                    substrate = playground,
                    bots = genBots nBots [(30, 30), (300, 500), (500, 200), (700, 400)],
                    players = dummyPlayers
                }

genBots :: Int -> [(Float, Float)] -> [Entity]
-- genBots = take nBots $ repeat $ genBot $ mkStdGen seed 
genBots _ [] = []
genBots 0 _ = []
genBots n centers = genBot n centers : genBots (n-1) centers

genBot :: Int -> [(Float, Float)] -> Entity
genBot i centers = Bot {
        stamina = stamina',
        style = style',
        perimeter = perimeter',
        strength = strength',
        velocity = velocity',
        awareness = awareness',
        reach = reach',
        position = position',
        direction = direction',
        flocking = flocking'
    }
    where
        iCenter = randomNumber (seed - i * 23247) 0 (length centers - 1) :: Int
        (cx, cy) = centers !! iCenter

        stamina' = randomNumber (seed - i) 20 100 :: Int
        encodedStyle = randomNumber (seed - i*2) 0 2 :: Int
        style'
          | encodedStyle == 0 = "aggresive"
          | encodedStyle == 1 = "devensive"
          | otherwise = "balanced" :: String

        perimeter' = randomNumber (seed - i*3) 50 150 :: Float
        -- perimeter' = 100 :: Float

        strength' = randomNumber (seed - i*4) 3 10 :: Int
        awareness' = randomNumber (seed - i*6) 3 10 :: Int
        reach' = randomNumber (seed - i*7) 5 50 :: Int

        position'XDelta = randomNumber (seed - i*8393) (-50) 50
        position'YDelta = randomNumber (seed - i*82933) (-50) 50
        position' = (cx + position'XDelta, cy + position'YDelta) :: (Float, Float)

        direction' = normalize' (randomNumber (seed - i*1120) (-1) 1, randomNumber (seed - i*11370) (-1) 1) :: (Float, Float)
        velocity' = randomNumber (seed - i*5) 20 50 :: Float
        -- velocity' = 500 :: Float
        flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool

------------------------------- Simulation -------------------------------    

simulateKI :: IO ()
simulateKI = simulate window background fps initialKIState render update

render :: KIState -> Picture
render state = picture
    where
        bots' = bots state
        players' = players state
        picture = pictures $ entitiesToPictures black bots' ++ entitiesToPictures red players'


entitiesToPictures :: Color -> [Entity] -> [Picture]
entitiesToPictures _ [] = []
entitiesToPictures _color (x:xs) = entityPicture : entitiesToPictures _color xs
    where
        pos = position x
        drawX = fst pos - fst midPlayground
        drawY = snd pos - snd midPlayground
        entityPicture = GLOSS.translate drawX drawY $ color _color $ circleSolid 3


update :: ViewPort -> Float -> KIState -> KIState
update _ = moveAgents

moveAgents :: Float -> KIState -> KIState
moveAgents seconds state = newState'
    where
        bots = view botsL state
        players = view playersL state
        newBots = over (botsL . traverse . movementL) (moveAgent seconds bots players) state
        newState' = newBots

moveAgent :: Float -> [Entity] -> [Entity] -> MovementAttr -> MovementAttr
moveAgent seconds bots players movementAttrs = newMovementAttrs'
    where
        newMovementAttrs = calcNewMovementAttrs seconds bots players $ countNeighbors movementAttrs bots -- 0 is initial no of neighbors
        newMovementAttrs' = updatePosition seconds movementAttrs newMovementAttrs

updatePosition :: Float -> MovementAttr -> MovementAttr -> MovementAttr
updatePosition seconds ((x,y), (_,_), v, p) ((_,_), (dirX',dirY'), _, _) = ((x'',y''), (dirX',dirY'), v, p)
    where
        x'' = x + dirX' * v * seconds
        y'' = y + dirY' * v * seconds

countNeighbors :: MovementAttr -> [Entity] -> (MovementAttr, Int, [Int])
countNeighbors mvnt@((x,y), (x',y'), v, p) bots = (mvnt, count, indezes)
    where
        positions = map (view positionL) bots
        indezes = map fst (findItems ((>=) p . distance (x, y)) positions)
        count = length indezes

-- first: pretend all have flocking behaviour
calcNewMovementAttrs :: Float -> [Entity] -> [Entity] -> (MovementAttr, Int, [Int]) -> MovementAttr
calcNewMovementAttrs seconds bots players (movementAttrs, n, ixs) = newMovementAttrs
    where
        relevantBots = map (fst . snd) (findItems (flip elem ixs . snd) (zip bots [0..]))

        -- playersPos = toListOf (traverse . positionL) players
        -- closePlayers = filter (tComp (<=)
        --                             (view movementPerimeterL movementAttrs)
        --                             (view movementPositionL movementAttrs)
        --                             . view positionL) players

        nearestPlayerI = fst (minimumBy (compare `on` snd) (zip [0..] (map (distance (view movementPositionL movementAttrs) . view positionL) players)))
        nearestPlayer = players !! nearestPlayerI
        inPerimeter = view movementPerimeterL movementAttrs <= distance (view positionL nearestPlayer) (view movementPositionL movementAttrs)

        alignment = normalizeA n $ foldr addToMovementsAttrsAlignment movementAttrs relevantBots
        cohesion = normalizeC n $ foldr addToMovementsAttrsCohesion movementAttrs relevantBots
        playerAwareness = normalizeP $ addToMovementsAttrsPlayerAwareness movementAttrs nearestPlayer
        separation = normalizeS n $ foldr addToMovementsAttrsSeparation movementAttrs relevantBots
        newMovementAttrs
            | inPerimeter = normalizeF $ foldr combineSteering movementAttrs [alignment, cohesion, separation]
            | otherwise = normalizeF $ combineSteering movementAttrs playerAwareness

addToMovementsAttrsAlignment :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsAlignment bot ((x,y), (dirX,dirY), v, p) = mvmnt'
    where
        (botDirX, botDirY) = view directionL bot
        mvmnt' = ((x,y), (dirX+botDirX,dirY+botDirY), v, p)

addToMovementsAttrsCohesion :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsCohesion bot ((x,y), (dirX,dirY), v, p) = mvmnt'
    where
        (botPosX, botPosY) = view positionL bot
        mvmnt' = ((x,y), (dirX+botPosX,dirY+botPosY), v, p)

addToMovementsAttrsSeparation :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsSeparation bot ((x,y), (dirX,dirY), v, p) = mvmnt'
    where
        (botPosX, botPosY) = view positionL bot
        mvmnt' = ((x,y), (dirX+botPosX-x,dirY+botPosY-y), v, p)

addToMovementsAttrsPlayerAwareness :: MovementAttr -> Entity -> MovementAttr
addToMovementsAttrsPlayerAwareness ((x,y), (dirX,dirY), v, p) player = mvmnt'
    where
        (playerX, playerY) = view positionL player
        mvmnt' = ((x,y), (dirX+playerX,dirY+playerY), v, p)

normalizeA :: Int -> MovementAttr -> MovementAttr
normalizeA n = over movementDirectionL $ normalizeWeighted 2

normalizeC :: Int -> MovementAttr -> MovementAttr
normalizeC n mvmnt = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos . tApp2 (/) (fromIntegral n,fromIntegral n)) mvmnt
    where
        pos = view movementPositionL mvmnt

normalizeS :: Int -> MovementAttr -> MovementAttr
normalizeS n = over movementDirectionL (normalizeWeighted 1 . tApp1 (*(-1)))

normalizeF :: MovementAttr -> MovementAttr
normalizeF = over movementDirectionL normalize'

normalizeP :: MovementAttr -> MovementAttr
normalizeP mvmnt = over movementDirectionL (normalizeWeighted 1 . (tApp2 (-) `flip`) pos) mvmnt
    where
        pos = view movementPositionL mvmnt

combineSteering :: MovementAttr -> MovementAttr -> MovementAttr
combineSteering ((x,y), (dirX,dirY), v, p) ((_,_), (dirX',dirY'), _, _) = ((x,y), (dirX+dirX',dirY+dirY'), v, p)
