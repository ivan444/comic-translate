{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import Prelude hiding (readFile)
import System.Environment
import Data.Array.MArray
import Data.ByteString hiding (putStrLn, unpack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Unsafe
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Encoding as T
import Data.Word
import Foreign.Marshal.Alloc
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Windows.OffscreenWindow
import Graphics.UI.Gtk.Builder
import qualified System.Glib.UTFString as Glib
import qualified Data.Vector.Unboxed as V
import Graphics.UI.Gtk.Gdk.DrawWindow
       (drawWindowGetWidth, drawWindowGetHeight)
import Graphics.UI.Gtk.Abstract.Widget
       (widgetSizeRequest, widgetTranslateCoordinates)
import Data.Maybe (fromMaybe)
import qualified Codec.Picture.Png as JP
import qualified Codec.Picture.Types as JP
import qualified Data.Map as Map
import Data.Map ((!))
import HFlags
import Comic.OCR
import Comic.Translate
import Paths_comictrans (getDataFileName)
import qualified System.Console.ANSI as ColTerm

defineFlag "ocr_lang" ("eng" :: T.Text) "Language of a text we want to OCR."

defineFlag
    "translation_source_lang"
    ("en" :: T.Text)
    "Language to be translated."

defineFlag
    "translation_destination_lang"
    ("de" :: T.Text)
    "Language to which the test will be translated."

defineFlag
    "tesseract_data_dir"
    ("/home/ikr/radni" :: T.Text)
    "Path to the directory with Tesseract language data."

$(return []) -- workaround for https://github.com/nilcons/hflags/issues/8

data GUI = GUI
    { win :: Window
    , source :: Image
    } 

yandexApiKey = 
    "trnsl.1.1.20141206T232937Z.afa78ec902bc2385.64360501ae9af320dd9d69ccb190b091299abc2f"

-- | Start GUI.
main
    :: IO ()
main = do
    $initHFlags "Desktop app for realtime translation of web comics"
    initGUI
    -- Load the GUI from the XML file
    builder <- builderNew
    guiXmlFilePath <- getDataFileName "gui/comic-translate.xml"
    guiXml <- B.readFile guiXmlFilePath
    builderAddFromString builder $ T.decodeUtf8 guiXml
    gui <- buildGUI builder
    bindGuiEvents gui (YandexClient yandexApiKey)
    window <- builderGetObject builder castToWindow ("translatorWin" :: String)
    on window deleteEvent $ liftIO (mainQuit >> return False)
    widgetShowAll window
    mainGUI

buildGUI :: Builder -> IO GUI
buildGUI builder = do
    win <- getWidget castToWindow "translatorWin"
    source <- getWidget castToImage "sourceImg"
    return
        GUI
        { win = win
        , source = source
        }
  where
    getWidget
        :: GObjectClass cls
        => (GObject -> cls) -> String -> IO cls
    getWidget = builderGetObject builder

bindGuiEvents
    :: Translator a
    => GUI -> a -> IO HandlerId
bindGuiEvents gui translator = do
    Just screen <- screenGetDefault
    timeoutAdd (captureScreenshot gui) 25
    timeoutAdd (translateText gui translator) 1000

-- | Helper for taking screenshot in every time interval.
captureScreenshot
    :: GUI -> IO Bool
captureScreenshot gui = do
    setSourceImage gui
    return True

-- | Take screenshot and set it in the image GUI widget.
setSourceImage
    :: GUI -> IO ()
setSourceImage gui = screenShot gui >>= imageSetFromPixbuf (source gui)

-- | Ensure value is between given limits (mn and mx)
ensureLimits
    :: Ord a
    => a -> a -> a -> a
ensureLimits x mn mx = min mx (max x mn)

-- | Compute correct size from origin, given size and maximum size.
computeSize
    :: (Num a, Ord a)
    => a -> a -> a -> a
computeSize o s m = 
    if o + s <= m
        then s
        else m - o

overlap
    :: (Num a, Ord a)
    => a -> a -> a -> a -> Bool
overlap x xw o ow = 
    or [and [(x <= o), (x + xw >= o)], and [(x <= o + ow), (x + xw >= o + ow)]]

-- | Take a screenshot of a portion of a screen around the mouse pointer.
screenShot
    :: GUI -> IO Pixbuf
screenShot gui = do
    -- Get screen size.
    Just screen <- screenGetDefault
    window <- screenGetRootWindow screen
    (mw,mh) <- drawableGetSize window
    -- Get current pointer position.
    (_,x,y,_) <- drawWindowGetPointerPos window
    -- Handle overlap of source rectangle and draw window.
    -- Get widget dimensions.
    ws@(ww,wh) <- widgetGetSize (source gui)
    -- Get widget origin (coordinates of upper left corner).
    (wx,wy) <- widgetGetDrawWindow (source gui) >>= drawWindowGetOrigin
    -- Compute screenshot origin and size so that
    -- mouse pointer is in the middle.
    let origin@(ox,oy) = 
            ( ensureLimits (x - ww `div` 2) 0 mw
            , ensureLimits (y - wh `div` 2) 0 mh)
        size = (computeSize ox ww mw, computeSize oy wh mh)
    Just pxbuf <- 
        pixbufGetFromDrawable
            window
            ((uncurry . uncurry Rectangle) origin size)
    if and [overlap wx ww ox ww, overlap wy wh oy wh]
        then do
            imgPxbf <- imageToPixbuf (source gui)
            case imgPxbf of
                Just px -> return px
                Nothing -> return pxbuf
        else return pxbuf

-- | Take OCR'd text from GUI and translate it by using translation web service.
translateText
    :: Translator a
    => GUI -> a -> IO Bool
translateText gui translator = do
    let img = (source gui)
    forkIO $
        do t <- ocrGuiImage img
           transText <- 
               translate
                   translator
                   flags_translation_source_lang
                   flags_translation_destination_lang
                   t
           ColTerm.setSGR
               [ ColTerm.SetColor
                     ColTerm.Foreground
                     ColTerm.Dull
                     ColTerm.Magenta]
           T.putStrLn t
           ColTerm.setSGR
               [ColTerm.SetColor ColTerm.Foreground ColTerm.Vivid ColTerm.Red]
           T.putStrLn transText
           ColTerm.setSGR [ColTerm.Reset]
    return True

imageToPixbuf :: Image -> IO (Maybe Pixbuf)
imageToPixbuf img = do
    imageType <- get img imageStorageType
    if imageType == ImagePixbuf
        then do
            pxbf <- imageGetPixbuf img
            return $ Just pxbf
        else return Nothing

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

ocrGuiImage :: Image -> IO Text
ocrGuiImage img = do
    imgPxbf <- imageToPixbuf img
    case imgPxbf of
        Just px -> do
            imgContent <- pixbufBytes px
            ocrImage imgContent flags_tesseract_data_dir flags_ocr_lang
        Nothing -> return $ T.pack "-"
