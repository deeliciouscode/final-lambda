module KI.KI where

import KI.Structures
import KI.Config
import System.Random
import KI.Lenses
import Graphics.Gloss as GLOSS
import Graphics.Gloss.Data.ViewPort
import Control.Lens
import Helpers

debugKI :: IO()
debugKI = do
    print playground
    print $ genBots nBots
    return ()

initialKIState :: KIState
initialKIState = State {
                    substrate = playground,
                    bots = genBots nBots,
                    players = dummyPlayers
                }

genBots :: Int -> [Entity]
-- genBots = take nBots $ repeat $ genBot $ mkStdGen seed 
genBots 0 = []
genBots n = genBot n : genBots (n-1)

genBot :: Int -> Entity
genBot i = Bot {
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
        randomNumber :: (Random a) => Int -> a -> a -> a
        randomNumber seed min max = fst $ randomR (min,max) (mkStdGen $ seed)

        stamina' = randomNumber (seed - i) 20 100 :: Int
        encodedStyle = randomNumber (seed - i*2) 0 2 :: Int
        style'
          | encodedStyle == 0 = "aggresive"
          | encodedStyle == 1 = "devensive"
          | otherwise = "balanced" :: String
        perimeter' = randomNumber (seed - i*3) 50 250 :: Int
        strength' = randomNumber (seed - i*4) 3 10 :: Int
        awareness' = randomNumber (seed - i*6) 3 10 :: Int
        reach' = randomNumber (seed - i*7) 5 50 :: Int
        position' = (randomNumber (seed - i*8) 0 960, randomNumber (seed - i*9) 0 540) :: (Float, Float)
        direction' = (randomNumber (seed - i*10) (-1) 1, randomNumber (seed - i*11) (-1) 1) :: (Float, Float)
        velocity' = randomNumber (seed - i*5) 5 10 :: Float
        flocking' = randomNumber (seed - i*12) (0 :: Int) (1 :: Int) == 1 :: Bool

dummyPlayers :: [Entity]
dummyPlayers = []

------------------------------- Simulation -------------------------------    

simulateKI :: IO ()
simulateKI = simulate window background fps initialKIState render update

render :: KIState -> Picture
render state = picture
    where
        bots' = bots state
        players' = players state
        picture = pictures $ entitiesToPictures bots' ++ entitiesToPictures players'

entitiesToPictures :: [Entity] -> [Picture]
entitiesToPictures [] = []
entitiesToPictures (x:xs) = entityPicture : entitiesToPictures xs
    where
        pos = position x
        drawX = fst pos - fst midPlayground
        drawY = snd pos - snd midPlayground
        entityPicture = GLOSS.translate drawX drawY $ color black $ circleSolid 3


update :: ViewPort -> Float -> KIState -> KIState
update _ = moveAgents

moveAgents :: Float -> KIState -> KIState
moveAgents seconds state = newState'
    where
        newState' = undefined
        bots = view botsL state
        players = view playersL state
        newBots = over (botsL . traverse . movementL) (moveAgent seconds bots players) state

moveAgent :: Float -> [Entity] -> [Entity] -> MovementAttr -> MovementAttr
moveAgent seconds bots players movementAttrs = newMovementAttrs
    where
        newMovementAttrs = calcNewMovementAttrs seconds bots players $ countNeighbors movementAttrs bots -- 0 is initial no of neighbors

countNeighbors :: MovementAttr -> [Entity] -> (MovementAttr, Int, [Int])
countNeighbors mvnt@((x,y), (x',y'), v, p) bots = (mvnt, count, indezes)
    where
        positions = map (view positionL) bots
        indezes = map fst (findItems ((>=) (fromIntegral p) . distance (x, y)) positions)
        count = length indezes

-- first: pretend all have flocking behaviour
calcNewMovementAttrs :: Float -> [Entity] -> [Entity] -> (MovementAttr, Int, [Int]) -> MovementAttr
calcNewMovementAttrs seconds bots players (movementAttrs, n, ixs) = newMovementAttrs
    where
        newMovementAttrs = movementAttrs
        relevantBots = map (fst . snd) (findItems (flip elem ixs . snd) (zip bots [0..]))
        alignment = normalizeA n $ foldr addToMovementsAttrsAlignment movementAttrs relevantBots
 
addToMovementsAttrsAlignment :: Entity -> MovementAttr -> MovementAttr
addToMovementsAttrsAlignment bot ((x,y), (dirX,dirY), v, p) = mvmnt'
    where
        (botDirX, botDirY) = view directionL bot
        mvmnt' = ((x,y), (dirX+botDirX,dirY+botDirY), v, p)

normalizeA :: Int -> MovementAttr -> MovementAttr 
normalizeA n mvmnt = mvmnt'
    where
        mvmnt' = mvmnt

-- align :: Agents -> Agents
-- align agents = normalizeAlignment $ countNeigborsWith agents calcAlignmentVector

-- calcAlignmentVector :: Agent -> Agents -> Agent
-- calcAlignmentVector agent [] = agent
-- calcAlignmentVector agent@(r, (x,y), (vx,vy), n) ((r', (x',y'), (vx',vy'), _):rest)
--                     | r == r' && x == x' && y == y' = calcAlignmentVector agent rest
--                     | distance (x,y) (x',y') <= neighborThreshold = calcAlignmentVector (r, (x,y), (vx + vx',vy + vy'), n+1) rest
--                     | otherwise = calcAlignmentVector agent rest

-- normalizeAlignment :: Agents -> Agents
-- normalizeAlignment [] = []
-- normalizeAlignment ((r, (x,y), (vx,vy), n):rest) = (r, (x,y), (vx',vy'), n) : normalizeAlignment rest
--                                 where
--                                     nF = fromIntegral n :: Float
--                                     (vx',vy') = if n /= 0 then normalize' (vx/nF, vy/nF)
--                                                 else (0, 0)
