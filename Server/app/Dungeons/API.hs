module Dungeons.API where

import Dungeons.Gen
import Dungeons.Transform
import Dungeons.Config
import Graphics.Gloss.Game
import Graphics.Image
import Data.Vector.Storable as VS
import Graphics.Gloss.Export
import Codec.Picture.Png.Internal.Export
import Text.Printf
import Dungeons.Structures


getDungeon :: Int -> IO ((Int, Int), VS.Vector Int, ((Float, Float), Circles))
getDungeon seed = do
    (pic, entry, botPositions) <- generateDungeon seed sideLen

    let imagePath = printf "images/dungeon%d.png" seed
    exportPictureToFormat writePng (round sideLen, round sideLen) black imagePath pic

    let meta = (entry, botPositions) 
    cluster <- readImageRGB VS imagePath
    -- let dungeonImage = png "images/gloss_new.png"
    (dims, vector) <- transformToVector cluster
    return (dims, vector, meta)