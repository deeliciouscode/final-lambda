module KI.KI where

import KI.Structures 
import KI.Config
import System.Random
import KI.Lenses

debugKI :: IO()
debugKI = do
    print playground
    print $ genBots nBots
    return ()

init :: KIState
init = State {
        substrate = playground,
        bots = genBots nBots,
        players = dummyPlayers   
    }

genBots :: Int -> [Entity]
-- genBots = take nBots $ repeat $ genBot $ mkStdGen seed 
genBots 0 = []  
genBots n = (genBot n) : genBots (n-1)

genBot :: Int -> Entity
genBot i = Bot {
        stamina = stamina',
        style = style',
        perimeter = perimeter',
        strength = strength',
        speed = speed',
        awareness = awareness',
        reach = reach',
        position = position',
        direction = direction',
        flocking = flocking'
    }
    where
        randomNumber :: Int -> Int -> Int -> Int
        randomNumber seed min max = fst $ randomR (min,max) (mkStdGen $ seed)
        
        stamina' = randomNumber (seed - i) 20 100 :: Int
        encodedStyle = randomNumber (seed - i*2) 0 2 :: Int
        style' = if encodedStyle == 0 then "aggresive" 
                else if encodedStyle == 1 then "devensive" 
                else "balanced" :: (String)
        perimeter' = randomNumber (seed - i*3) 50 250 :: Int
        strength' = randomNumber (seed - i*4) 3 10 :: Int
        speed' = randomNumber (seed - i*5) 5 10 :: Int
        awareness' = randomNumber (seed - i*6) 3 10 :: Int
        reach' = randomNumber (seed - i*7) 5 50 :: Int
        position' = (randomNumber (seed - i*8) 0 960, randomNumber (seed - i*9) 0 540) :: (Int, Int)
        direction' = (randomNumber (seed - i*10) (-1) 1, randomNumber (seed - i*11) (-1) 1) :: (Int, Int)
        flocking' = randomNumber (seed - i*12) 0 1 == 1 :: Bool

dummyPlayers :: [Entity]
dummyPlayers = []


-------------------------------------------------------------------------------------------


-- ------------------------------- Flocking -------------------------------    

-- circles'' = flockCircles circles

-- flockCircles :: Circles -> Circles
-- flockCircles = stripCircles . iterateOver 0 . enrichCircles

-- enrichCircles :: Circles -> Agents
-- enrichCircles [] = []
-- enrichCircles ((r, (x,y)):rest) = (r, (x,y), (vx',vy'), 0) : enrichCircles rest
--                     where
--                         gen = mkStdGen $ round (420 / x + y) :: StdGen
--                         (vx, gen') = randomR (-1,1) gen :: (Float, StdGen)
--                         (vy, _) = randomR (-1,1) gen' :: (Float, StdGen)
--                         (vx', vy') = normalize' (vx, vy)

-- stripCircles :: Agents -> Circles
-- stripCircles [] = []
-- stripCircles ((r, (x,y), (vx,vy), n):rest) = (r, (x,y)) : stripCircles rest


-- ------------------------------- Flocking.Iteration -------------------------------    

-- -- try out best combinations of alignment, cohesion and separation
-- iterateOver :: Int -> Agents -> Agents
-- iterateOver n agents
--             | n >= iterations = agents
--             | otherwise       = iterateOver (n + 1) (flock agents)

-- flock :: Agents -> Agents
-- flock agents = applyIteration $ LIST.transpose [agents, align agents, separate agents] -- [align agents, makeCoherent agents, separate agents]

-- flock' :: Agents -> Agents
-- flock' agents = applyIteration $ LIST.transpose [agents, align agents, makeCoherent agents, separate agents]

-- applyIteration :: [Agents] -> Agents
-- applyIteration = LIST.map applyVectors

-- applyVectors :: Agents -> Agent
-- applyVectors [] = (0, (0,0), (0,0), 0)
-- applyVectors (first:rest) = moveAgent $ normalizeAgent velocity $ foldIntoOne first rest

-- foldIntoOne :: Agent -> Agents -> Agent
-- foldIntoOne agent [] = agent
-- foldIntoOne (r, (x,y), (vx,vy), _) ((_, _, (vx',vy'), _):rest) = foldIntoOne (r, (x,y), (vx+vx',vy+vy'), 0) rest

-- normalizeAgent :: Float -> Agent -> Agent
-- normalizeAgent multiplier (r, (x,y), (vx,vy), n) = (r, (x,y), (vx'', vy''), n)
--                             where
--                                 (vx',vy') = normalize' (vx,vy)
--                                 vx'' = vx' * multiplier
--                                 vy'' = vy' * multiplier

-- moveAgent :: Agent -> Agent
-- moveAgent (r, (x,y), (vx,vy), n) = (r, (x+vx,y+vy), (vx,vy), n)

-- ------------------------------- Flocking.Neigbors -------------------------------    

-- countNeigborsWith :: Agents -> FlockingFunction -> Agents
-- countNeigborsWith agents = countNeigborsIWith agents 0

-- countNeigborsIWith :: Agents -> Int -> FlockingFunction -> Agents
-- countNeigborsIWith agents i f
--                     | i == length agents = agents
--                     | otherwise = countNeigborsIWith (countNeigborsIWith' agents i f) (i+1) f

-- countNeigborsIWith' :: Agents -> Int -> FlockingFunction -> Agents
-- countNeigborsIWith' agents i f = LIST.take i agents ++ (agent' : LIST.drop (i+1) agents)
--                     where
--                         agent' = f (agents !! i) agents


-- ------------------------------- Flocking.Alignment -------------------------------    

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


-- ------------------------------- Flocking.Cohesion -------------------------------    

-- makeCoherent :: Agents -> Agents
-- makeCoherent agents = normalizeCohesion $ countNeigborsWith agents calcCohesionVector

-- calcCohesionVector :: Agent -> Agents -> Agent
-- calcCohesionVector agent [] = agent
-- calcCohesionVector agent@(r, (x,y), (vx,vy), n) ((r', (x',y'), (vx',vy'), _):rest)
--                     | r == r' && x == x' && y == y' = calcCohesionVector agent rest
--                     | distance (x,y) (x',y') <= neighborThreshold = calcCohesionVector (r, (x,y), (vx + x',vy + y'), n+1) rest
--                     | otherwise = calcCohesionVector agent rest

-- normalizeCohesion :: Agents -> Agents
-- normalizeCohesion [] = []
-- normalizeCohesion ((r, (x,y), (vx,vy), n):rest) = (r, (x,y), (vx',vy'), n) : normalizeCohesion rest
--                                 where
--                                     nF = fromIntegral n :: Float
--                                     vxCenter = vx / nF  - x
--                                     vyCenter = vy / nF - y
--                                     (vx',vy') = if n /= 0 then normalize' (vxCenter, vyCenter)
--                                                 else (0, 0)


-- ------------------------------- Flocking.Separation -------------------------------    

-- separate :: Agents -> Agents
-- separate agents = normalizeSeparation $ countNeigborsWith agents calcSeparationVector

-- calcSeparationVector :: Agent -> Agents -> Agent
-- calcSeparationVector agent [] = agent
-- calcSeparationVector agent@(r, (x,y), (vx,vy), n) ((r', (x',y'), (vx',vy'), _):rest)
--                     | r == r' && x == x' && y == y' = calcSeparationVector agent rest
--                     | distance (x,y) (x',y') <= neighborThreshold = calcSeparationVector (r, (x,y), (vx'',vy''), n+1) rest
--                     | otherwise = calcSeparationVector agent rest
--                         where
--                             vx'' = vx + x' - x
--                             vy'' = vy + y' - y

-- normalizeSeparation :: Agents -> Agents
-- normalizeSeparation [] = []
-- normalizeSeparation ((r, (x,y), (vx,vy), n):rest) = (r, (x,y), (vx',vy'), n) : normalizeSeparation rest
--                                 where
--                                     nF = fromIntegral n :: Float
--                                     (vx',vy') = if n /= 0 then normalize' (-1*vx/nF, -1*vy/nF)
--                                                 else (0, 0)
