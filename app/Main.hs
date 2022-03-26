module Main where
    
import Dungeons.Gen
import Dungeons.Config
import Dungeons.Transform
import KI.KI
import KI.Debug
import Graphics.Gloss
import Graphics.Gloss.Game
import GHC.Real (Integral(toInteger))
import Graphics.Image
import Prelude as P


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
    P.putStrLn "executing main..."
    -- meta <- saveGloss
    -- print meta

    let meta = ((756.0964,767.90125),[(547.1509,587.6747),(314.9494,221.06418),(363.15738,169.11577),(354.1247,782.55493),(359.16797,813.0572),(330.0352,165.16618),(344.2131,633.53723),(655.18445,641.47534),(336.88705,809.55676),(668.95435,643.7055),(236.08603,449.1241),(686.1385,231.98135),(687.83405,692.10333),(387.7327,530.61957),(179.43361,732.58514),(542.57135,342.3235),(383.8749,631.3151),(208.37465,381.1288),(771.0372,594.44135),(233.75873,751.0682)])
    cluster <- readImageRGB VS "images/test_gloss.png"
    let dungeon = png "images/gloss_new.png"
    map <- transformToMatrix cluster
    playWithKI meta map dungeon  

    -- simulateKI
    -- return ()

    -- meta <- saveGlossDebug
    -- let meta = ((756.0964,767.90125),[(950,900)])
    -- cluster <- readImageRGB VS "images/calibration.png"
    -- map <- transformToMatrix cluster
    -- let dungeon = png "images/gloss_new.png"
    -- debugKI meta map dungeon
