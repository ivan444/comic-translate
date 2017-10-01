module Comic.Pixbuf
  (pixbufBytes)
  where

import Data.Array.MArray (getElems)
import Data.Map ((!))
import Data.Word (Word8)
import Graphics.UI.Gtk.Gdk.Pixbuf
import qualified Codec.Picture.Png as JP
import qualified Codec.Picture.Types as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as V

pixbufBytes :: Pixbuf -> IO B.ByteString
pixbufBytes pixbuf = do
    pbd <- pixbufGetPixels pixbuf :: IO (PixbufData Int Word8)
    ws <- getElems pbd
    width <- pixbufGetWidth pixbuf
    height <- pixbufGetHeight pixbuf
    nChannels <- pixbufGetNChannels pixbuf
    rowstride <- pixbufGetRowstride pixbuf
    let m = mapBytes ws width height nChannels rowstride
    let img = pixbufImage m width height
    return $ BL.toStrict $ JP.encodePng img

mapBytes :: [Word8]
         -> Int
         -> Int
         -> Int
         -> Int
         -> Map.Map (Int, Int) JP.PixelRGB8
mapBytes xs width height nChannels rowstride = Map.fromList pixelList
  where
    pixelList :: [((Int, Int), JP.PixelRGB8)]
    pixelList = 
        let vs = V.fromList xs
        in Prelude.map (buildPixel vs) coordinates
    buildPixel :: V.Vector Word8 -> (Int, Int) -> ((Int, Int), JP.PixelRGB8)
    buildPixel vs cord@(x,y) = 
        let p = pixelOffset x y
        in (cord, JP.PixelRGB8 (red p vs) (green p vs) (blue p vs))
    pixelOffset :: Int -> Int -> Int
    pixelOffset x y = y * rowstride + x * nChannels
    red :: Int -> V.Vector Word8 -> JP.Pixel8
    red p vs = vs V.! p
    green :: Int -> V.Vector Word8 -> JP.Pixel8
    green p vs = vs V.! (p + 1)
    blue :: Int -> V.Vector Word8 -> JP.Pixel8
    blue p vs = vs V.! (p + 2)
    coordinates :: [(Int, Int)]
    coordinates = 
        [ (x, y)
        | x <- [0 .. width - 1] 
        , y <- [0 .. height - 1] ]

pixbufImage :: Map.Map (Int, Int) JP.PixelRGB8
            -> Int
            -> Int
            -> JP.Image JP.PixelRGB8
pixbufImage m width height = JP.generateImage getPixel width height
  where
    getPixel :: Int -> Int -> JP.PixelRGB8
    getPixel x y = m ! (x, y)
