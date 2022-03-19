module KI.Lenses where

import KI.Structures
import Control.Lens
import Data.Maybe.HT
import Distribution.Utils.Generic
import Helpers

-- https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html#g:2
-- https://www.youtube.com/watch?v=3kduOmZ2Wxw

testPlayground :: [[Int]]
testPlayground = replicate 10 (replicate 6 0)

testBot :: Entity
testBot = Bot {
            stamina = 1,
            style = "aggresive",
            perimeter = 2,
            strength = 3,
            awareness = 5,
            reach = 6,
            position = (8,18),
            direction = (9,19),
            velocity = 4,
            flocking = True
        }

testPlayer :: Entity
testPlayer = Player {
            stamina = 10,
            strength = 30,
            position = (80,180),
            direction = (90,190),
            velocity = 40
        }

testState :: KIState
testState = State {
    substrate = testPlayground,
    bots = [testBot, testBot, testBot, testBot, testBot],
    players = [testPlayer, testPlayer, testPlayer, testPlayer, testPlayer]
}

staminaL :: Lens' Entity Int
staminaL = lens stamina (\entity newStamina -> entity { stamina = newStamina })

styleL :: Lens' Entity String
styleL = lens style (\bot newStyle -> bot { style = newStyle })

perimeterL :: Lens' Entity Int
perimeterL = lens perimeter (\bot newPerimeter -> bot { perimeter = newPerimeter })

strengthL :: Lens' Entity Int
strengthL = lens strength (\entity newStrength -> entity { strength = newStrength })

velocityL :: Lens' Entity Float
velocityL = lens velocity (\entity newVelocity-> entity { velocity = newVelocity })

awarenessL :: Lens' Entity Int
awarenessL = lens awareness (\bot newAwareness -> bot { awareness = newAwareness })

reachL :: Lens' Entity Int
reachL = lens reach (\bot newReach -> bot { reach = newReach })

--------------- Position ---------------
positionL :: Lens' Entity (Float, Float)
positionL = lens position (\entity newPosition -> entity { position = newPosition })

positionXL :: Lens' (Float, Float) Float
positionXL = lens fst (\(_, y) x -> (x,y))

positionYL :: Lens' (Float, Float) Float
positionYL = lens snd (\(x, _) y -> (x,y))

--------------- Direction ---------------
directionL :: Lens' Entity (Float, Float)
directionL = lens direction (\entity newDirection -> entity { direction = newDirection })

directionXL :: Lens' (Float, Float) Float
directionXL = lens fst (\(_, y) x -> (x,y))

directionYL :: Lens' (Float, Float) Float
directionYL = lens snd (\(x, _) y -> (x,y))

--------------- Movement ---------------
movementL :: Lens' Entity MovementAttr
movementL = lens movementAttrs (\entity newMovementAttrs -> entity { position = fstOf4 newMovementAttrs,
                                                                    direction = sndOf4 newMovementAttrs,
                                                                    velocity = trdOf4 newMovementAttrs,
                                                                    perimeter = frtOf4 newMovementAttrs })
    where
        movementAttrs :: Entity -> MovementAttr
        movementAttrs entity = (position entity, direction entity, velocity entity, perimeter entity)

movementDirectionL :: Lens' MovementAttr (Float, Float)
movementDirectionL = lens direction' (\(pos, _, v, p) newDirection -> (pos, newDirection, v, p))
    where 
        direction' :: MovementAttr -> (Float, Float)
        direction' (_, dir, _, _) = dir


--------------- Flocking ---------------
flockingL :: Lens' Entity Bool
flockingL = lens flocking (\bot newFlocking -> bot { flocking = newFlocking })

--------------- State ---------------
-- https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html#g:2
-- (over (playersL . traverse . staminaL) (+ 1000)) testState => erhöht jeden stamina wert in den playern um 1000
-- (set (playersL . traverse . staminaL) (1000)) testState => setzt jeden stamina wert in den playern auf 1000
-- view (positionL) testBot => show position tuple
-- view (positionL . positionXL) testBot => show x value of position

-- https://hackage.haskell.org/package/lens-5.1/docs/Data-List-Lens.html
-- https://hackage.haskell.org/package/utility-ht-0.0.16
-- (view playersL testState) ^? ix 3 => 3rd element in players list
-- (view playersL testState) & ix 3 .~ 69 => replace element at index 3 in players list with 69 if there
-- ((view playgroundL testState) ^? ix 7) ?-> (ix 4 .~ 2) => replace element at index 4 with 2 in list with index 7 

botsL :: Lens' KIState [Entity]
botsL = lens bots (\state newBots -> state { bots = newBots })

-- replaces a botAttribute
-- replaceBotAttrAt :: Int -> a -> Bot -> Maybe Bot
-- replaceBotAttrAt i newVal bot = newSubstrate
--         where 
--             attr = view botsL bot 
--             newSubstrate = case substate ^? ix c of 
--                 Nothing -> Nothing
--                 Just column ->  
--                     case column ^? ix r of
--                         Nothing -> Nothing
--                         Just row -> Just $ substate & ix c .~ (column & (ix r .~ newVal))

--------------

playersL :: Lens' KIState [Entity]
playersL = lens players (\state newPlayers -> state { players = newPlayers })

--------------

playgroundL :: Lens' KIState [[Int]]
playgroundL = lens substrate (\state newPlayground -> state { substrate = newPlayground })

-- replaces a value in a 2D List, => Nothing if index does not exist
replacePGAt :: Int -> Int -> Int -> Maybe [[Int]]
replacePGAt c r newVal = newSubstrate
        where
            substate = view playgroundL testState
            newSubstrate = case substate ^? ix c of
                Nothing -> Nothing
                Just column ->
                    case column ^? ix r of
                        Nothing -> Nothing
                        Just row -> Just $ substate & ix c .~ (column & (ix r .~ newVal))

-- replaces a value in a 2D List, => old list if index does not exist
replacePGAt' :: Int -> Int -> Int -> [[Int]]
replacePGAt' c r newVal = newSubstrate
        where
            substate = view playgroundL testState
            newSubstrate = case substate ^? ix c of
                Nothing -> substate
                Just column ->
                    case column ^? ix r of
                        Nothing -> substate
                        Just row -> substate & ix c .~ (column & (ix r .~ newVal))
