module KI.Lenses where

import KI.Structures
import Helpers
import Types

import Control.Lens
import Data.Maybe.HT
import Distribution.Utils.Generic
import Data.Vector.Storable
import Server.LibMessage

-- https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html#g:2
-- https://www.youtube.com/watch?v=3kduOmZ2Wxw

testPlayground :: Vector Int
testPlayground = empty

testBot :: Entity
testBot = Bot {
            stamina = 1,
            style = "aggresive",
            perimeter = 2,
            strength = 3,
            awareness = 5,
            reach = 6,
            homebase = (9,17),
            position = (8,18),
            direction = (9,19),
            velocity = 4,
            flocking = True
        }

testState :: KIState
testState = State {
    dims = (0,0),
    substrate = testPlayground,
    bots = [testBot, testBot, testBot, testBot, testBot]
}

staminaL :: Lens' Entity Int
staminaL = lens stamina (\entity newStamina -> entity { stamina = newStamina })

styleL :: Lens' Entity String
styleL = lens style (\bot newStyle -> bot { style = newStyle })

perimeterL :: Lens' Entity Float
perimeterL = lens perimeter (\bot newPerimeter -> bot { perimeter = newPerimeter })

strengthL :: Lens' Entity Int
strengthL = lens strength (\entity newStrength -> entity { strength = newStrength })

velocityL :: Lens' Entity Float
velocityL = lens velocity (\entity newVelocity-> entity { velocity = newVelocity })

awarenessL :: Lens' Entity Int
awarenessL = lens awareness (\bot newAwareness -> bot { awareness = newAwareness })

reachL :: Lens' Entity Int
reachL = lens reach (\bot newReach -> bot { reach = newReach })

--------------- Homebase ---------------
homebaseL :: Lens' Entity PointF
homebaseL = lens homebase (\entity newHomebase -> entity { homebase = newHomebase })

homebaseXL :: Lens' PointF Float
homebaseXL = lens fst (\(_, y) x -> (x,y))

homebaseYL :: Lens' PointF Float
homebaseYL = lens snd (\(x, _) y -> (x,y))

--------------- Position ---------------
positionL :: Lens' Entity PointF
positionL = lens position (\entity newPosition -> entity { position = newPosition })

positionXL :: Lens' PointF Float
positionXL = lens fst (\(_, y) x -> (x,y))

positionYL :: Lens' PointF Float
positionYL = lens snd (\(x, _) y -> (x,y))

--------------- Direction ---------------
directionL :: Lens' Entity PointF
directionL = lens direction (\entity newDirection -> entity { direction = newDirection })

directionXL :: Lens' PointF Float
directionXL = lens fst (\(_, y) x -> (x,y))

directionYL :: Lens' PointF Float
directionYL = lens snd (\(x, _) y -> (x,y))

--------------- Movement ---------------
movementL :: Lens' Entity MovementAttr
movementL = lens movementAttrs (\entity newMovementAttrs -> entity { position = fstOf5 newMovementAttrs,
                                                                    direction = sndOf5 newMovementAttrs,
                                                                    homebase = trdOf5 newMovementAttrs,
                                                                    velocity = frtOf5 newMovementAttrs,
                                                                    perimeter = fftOf5 newMovementAttrs })
    where
        movementAttrs :: Entity -> MovementAttr
        movementAttrs entity = (position entity, direction entity, homebase entity, velocity entity, perimeter entity)

movementPositionL :: Lens' MovementAttr PointF
movementPositionL = lens position' (\(_, dir, hb, v, p) newPosition -> (newPosition, dir, hb, v, p))
    where 
        position' :: MovementAttr -> PointF
        position' (pos, _, _, _, _) = pos

movementDirectionL :: Lens' MovementAttr PointF
movementDirectionL = lens direction' (\(pos, _, hb, v, p) newDirection -> (pos, newDirection, hb, v, p))
    where 
        direction' :: MovementAttr -> PointF
        direction' (_, dir, _, _, _) = dir

movementHomebaseL :: Lens' MovementAttr PointF
movementHomebaseL = lens direction' (\(pos, dir, _, v, p) newHomebase -> (pos, dir, newHomebase, v, p))
    where 
        direction' :: MovementAttr -> PointF
        direction' (_, dir, _, _, _) = dir

movementVelocityL :: Lens' MovementAttr Float
movementVelocityL = lens velocity' (\(pos, dir, hb, _, p) newVelocity -> (pos, dir, hb, newVelocity, p))
    where 
        velocity' :: MovementAttr -> Float
        velocity' (_, _, _, vel, _) = vel

movementPerimeterL :: Lens' MovementAttr Float
movementPerimeterL = lens perimeter' (\(pos, dir, hb, v, _) newPerimeter -> (pos, dir, hb, v, newPerimeter))
    where 
        perimeter' :: MovementAttr -> Float
        perimeter' (_, _, _, _, per) = per


playerMovementL :: Lens' PlayerInfo PlayerMovementAttr
playerMovementL = lens movementAttrs (\entity newMovementAttrs -> entity { pI_position = fstOf3 newMovementAttrs,
                                                                            pI_direction = sndOf3 newMovementAttrs,
                                                                            pI_velocity = trdOf3 newMovementAttrs })
    where
        movementAttrs :: PlayerInfo -> PlayerMovementAttr
        movementAttrs player = (pI_position player, pI_direction player, pI_velocity player)

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

botsL :: Lens' KIState Entities
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

dimsL :: Lens' KIState (Int,Int)
dimsL = lens dims (\state newDims -> state { dims = newDims })

--------------

playersDebugL :: Lens' KIStateDebug [PlayerInfo]
playersDebugL = lens playersD (\state newPlayers -> state { playersD = newPlayers })

directionDebugL :: Lens' PlayerInfo PointF
directionDebugL = lens pI_direction (\entity newDirection -> entity { pI_direction = newDirection })

playerPositionL :: Lens' PlayerInfo PointF
playerPositionL = lens pI_position (\entity newPosition -> entity { pI_position = newPosition })

--------------

playgroundL :: Lens' KIState (Vector Int)
playgroundL = lens substrate (\state newPlayground -> state { substrate = newPlayground })

