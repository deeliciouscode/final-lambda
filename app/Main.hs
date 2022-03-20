module Main where
    
import Dungeons.Gen
import Dungeons.Config
import Dungeons.Transform
import KI.KI
import Graphics.Gloss
import GHC.Real (Integral(toInteger))
import Graphics.Image

width, height, offset :: Int
width = round sideLen
height = round sideLen
offset = 0

window :: Display
window = InWindow "Dungeon" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = do
    putStrLn "executing main..."
    -- saveGloss
    -- cluster <- readImageRGB VU "images/test_gloss.png"
    -- transformToMatrix cluster
    -- debugKI
    -- simulateKI
    playWithKI 


    -- display window background objects 
