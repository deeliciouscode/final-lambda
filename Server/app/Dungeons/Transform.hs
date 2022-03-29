module Dungeons.Transform where

import Graphics.Image.Interface as IF
import Graphics.Image
import Data.Vector.Unboxed.Base as VSB
import Prelude as P
import Data.Maybe
import Data.Vector.Unboxed
import Data.Vector.Storable as VS
import qualified Graphics.Image as GI
import Types


transformToVector :: Image VS RGB Double -> IO (PointI, VS.Vector Int)
transformToVector image = do
                            let imageY = toImageY image
                            let newImage = IF.imap (colorPixel imageY) imageY
                            let dimensions = dims newImage
                            let asVector = transformToIntVec $ toVector newImage
                            -- let pxl = asVector ! fromIx 20 (1,2)
                            -- writeImageExact PNG [] "images/gloss_new.png" (toWord8I newImage)
                            return (dimensions, asVector)

transformToIntVec :: VS.Vector (GI.Pixel GI.Y Double) -> VS.Vector Int
transformToIntVec = VS.map (\(GI.PixelY val) -> round val)

colorPixel :: Image VS Y Double -> (Int,Int) ->  Pixel Y Double -> Pixel Y Double
colorPixel image (i,j) p = if allNeigborsSameAs p then p else PixelY 0.5
                    where
                        neighbors = [(i+1,j-1),(i+1,j),(i+1,j+1),(i-1,j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1)]
                        allNeigborsSameAs :: Pixel Y Double -> Bool
                        allNeigborsSameAs p' = P.all (== p') (catMaybes [maybeIndex image coords | coords <- neighbors])

testVector :: IO (VS.Vector Int)
testVector = do
    imgD <- readImageRGB VS "images/calibration.png"
    let imageY = toImageY imgD
    let newImage = IF.imap (colorPixel imageY) imageY
    let dimensions = dims newImage
    let asVector = toVector newImage
    let vecInt = VS.map (\(PixelY val) -> round val) asVector
    return vecInt

testVectorColor :: IO (IF.Vector VS (Pixel RGB Double))
testVectorColor = do
    imgD <- readImageRGB VS "images/calibration.png"
    let asVector = toVector imgD
    return asVector
