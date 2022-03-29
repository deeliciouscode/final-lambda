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
import Types


getDungeon :: Int -> IO (PointI, VS.Vector Int, (PointF, Circles))
getDungeon seed = do
    (pic, entry, botPositions) <- generateDungeon seed sideLen
    let imagePath = printf "images/dungeon%d.png" seed
    exportPictureToFormat writePng (round sideLen, round sideLen) black imagePath pic
    let meta = (entry, botPositions) 
    cluster <- readImageRGB VS imagePath
    (dims, vector) <- transformToVector cluster
    return (dims, vector, meta)