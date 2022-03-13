module Main where
    
import Dungeons.Gen
import Graphics.Gloss
import GHC.Real (Integral(toInteger))

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
    putStrLn "Will execute saveGloss"
    saveGloss
    -- display window background objects 
