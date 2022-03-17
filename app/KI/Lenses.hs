module KI.Lenses where

import KI.Structures 
import Control.Lens

-- https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html#g:2
-- https://www.youtube.com/watch?v=3kduOmZ2Wxw

testPlayground :: Playground
testPlayground = (take 960 $ repeat 0, take 540 $ repeat 0)

testBot :: Entity
testBot = Bot {
            stamina = 1,
            style = "aggresive",
            perimeter = 2,
            strength = 3,
            speed = 4,
            awareness = 5,
            reach = 6,
            position = (8,18),
            direction = (9,19),
            flocking = True
        }

testPlayer :: Entity
testPlayer = Player {
            stamina = 10,
            strength = 30,
            speed = 40,
            position = (80,180),
            direction = (90,190)
        }

testState :: KIState
testState = State {
    substrate = testPlayground,
    bots = [testBot],
    players = [testPlayer]
}

staminaL :: Lens' Entity Int
staminaL = lens stamina (\entity newStamina -> entity { stamina = newStamina })

styleL :: Lens' Entity String
styleL = lens style (\bot newStyle -> bot { style = newStyle })

perimeterL :: Lens' Entity Int 
perimeterL = lens perimeter (\bot newPerimeter -> bot { perimeter = newPerimeter })

strengthL :: Lens' Entity Int
strengthL = lens strength (\entity newStrength -> entity { strength = newStrength })

speedL :: Lens' Entity Int 
speedL = lens speed (\entity newSpeed -> entity { speed = newSpeed })

awarenessL :: Lens' Entity Int
awarenessL = lens awareness (\bot newAwareness -> bot { awareness = newAwareness })

reachL :: Lens' Entity Int 
reachL = lens reach (\bot newReach -> bot { reach = newReach })

--------------- Position ---------------
positionL :: Lens' Entity (Int, Int) 
positionL = lens position (\entity newPosition -> entity { position = newPosition })

positionXL :: Lens' (Int, Int) Int 
positionXL = lens fst (\(_, y) x -> (x,y))

positionYL :: Lens' (Int, Int) Int 
positionYL = lens snd (\(x, _) y -> (x,y))

--------------- Direction ---------------
directionL :: Lens' Entity (Int, Int)
directionL = lens direction (\entity newDirection -> entity { direction = newDirection })

directionXL :: Lens' (Int, Int) Int 
directionXL = positionXL

directionYL :: Lens' (Int, Int) Int 
directionYL = positionYL

--------------- Flocking ---------------
flockingL :: Lens' Entity Bool
flockingL = lens flocking (\bot newFlocking -> bot { flocking = newFlocking })

--------------- State ---------------

botsL :: Lens' KIState [Entity]
botsL = lens bots (\state newBots -> state { bots = newBots })

playersL :: Lens' KIState [Entity]
playersL = lens players (\state newPlayers -> state { players = newPlayers })
