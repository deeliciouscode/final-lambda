import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.Set as S
import Data.Map as M
import Control.Monad (forM_)

type TileList = IO [Picture]

tileSheed :: FilePath -> TileList
tileSheed path = loadBMP path >>= \(Bitmap bitmapData) -> return $
            Prelude.map (\ coord  -> bitmapSection (Rectangle coord (32, 32) ) bitmapData)
            (let    bottom = snd size - div (snd size) 32 * 32
                    size = bitmapSize bitmapData in
                    [(x, y) |
                        x <- [0 .. fst size],
                        x `mod` 32 == 0,
                        y <- [bottom .. snd size],
                        (y - bottom) `mod` 32 == 0,
                        x + 32 <= fst size,
                        y + 32 <= snd size])



tileSheedToMap :: FilePath -> IO (Map (Int, Int) Picture)
tileSheedToMap path = do
    Bitmap bitmapData <- loadBMP path
    let size = bitmapSize bitmapData
        bottom = snd size - div (snd size) 32 * 32
        coords = [(x, y) |
                        x <- [0 .. fst size],
                        x `mod` 32 == 0,
                        y <- [bottom .. snd size],
                        (y - bottom) `mod` 32 == 0,
                        x + 32 <= fst size,
                        y + 32 <= snd size]
        empty = empty :: Map (Int,Int)  Picture
        _insert _map [] = _map
        _insert _map (x@(_x, _y):xs)= _insert (M.insert (div _x 32, div _y 32) (bitmapSection (Rectangle x (32,32)) bitmapData) _map) xs

    return $ _insert empty coords



main = do

    l <- tileSheed "images/tilesheet.bmp"

    playIO
        (InWindow "WindowName" (600,100) (10,10))
        (makeColor 1 1 1 1)
        60
        () --initialState
        (\s -> do
            let     performTranslate _ [] = []
                    performTranslate n (x:xs) = translate (n*40-250) 0 x  : performTranslate (n+1)  xs

            return  $ pictures $ performTranslate 0 l


            )
        (\e s -> return s)
        (\f s -> return s)

