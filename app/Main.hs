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

    let meta = ((756.0964,767.90125),[(88.83506,(547.1509,587.6747)),(72.48627,(314.9494,221.06418)),(62.582375,(363.15738,169.11577)),(62.114113,(354.1247,782.55493)),(58.244858,(359.16797,813.0572)),(58.081177,(330.0352,165.16618)),(57.610226,(344.2131,633.53723)),(55.314163,(655.18445,641.47534)),(54.78929,(336.88705,809.55676)),(50.955086,(668.95435,643.7055)),(50.939415,(236.08603,449.1241)),(50.906757,(686.1385,231.98135)),(46.632668,(687.83405,692.10333)),(45.255173,(387.7327,530.61957)),(45.110275,(179.43361,732.58514)),(44.469566,(542.57135,342.3235)),(41.188137,(383.8749,631.3151)),(41.14777,(208.37465,381.1288)),(41.1445,(771.0372,594.44135)),(40.86997,(233.75873,751.0682)),(40.136963,(568.4242,830.293)),(39.142643,(210.4319,297.0638)),(39.031273,(444.33856,494.58423)),(38.350616,(713.8021,616.9759)),(38.225906,(363.00406,838.13007)),(37.08574,(363.42157,574.71796)),(36.84659,(522.4305,751.21216)),(35.170574,(193.56978,719.96014)),(33.632538,(358.8055,805.36914)),(33.472195,(721.2673,778.1685))])
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
