module Dungeons.Transform where

import Graphics.Image.Interface as IF
import Graphics.Image
import Graphics.Image.Types
import Dungeons.Config
import Data.Vector.Unboxed.Base
import Prelude as P
import Data.Maybe

transformToMatrix :: Image VU RGB Double -> IO ()
transformToMatrix image = writeImage "images/gloss_new.png" $ IF.imap (colorPixel image) image

colorPixel :: Image VU RGB Double -> (Int,Int) ->  Pixel RGB Double -> Pixel RGB Double
colorPixel image (i,j) p = if allNeigborsSameAs p then p else (PixelRGB 0.5 0.5 0.5)
                    where 
                        neighbors = [(i+1,j-1),(i+1,j),(i+1,j+1),(i-1,j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1)]
                        allNeigborsSameAs :: Pixel RGB Double -> Bool
                        allNeigborsSameAs p' = P.and $ P.map (== p') $ catMaybes [maybeIndex image coords | coords <- neighbors]

