{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans (liftIO)
import Prelude hiding (readFile)
import System.Environment

import Data.Array.MArray
import Data.ByteString hiding (putStrLn, unpack)
import Data.ByteString.Unsafe
import Data.Text as T
import Data.Text.Encoding as T
import Data.Word
import Foreign.Marshal.Alloc
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Builder
import qualified System.Glib.UTFString as Glib
import Graphics.UI.Gtk.Gdk.DrawWindow (drawWindowGetWidth, drawWindowGetHeight)
import Graphics.UI.Gtk.Abstract.Widget (widgetSizeRequest, widgetTranslateCoordinates)
import Data.Maybe (fromMaybe)

import HFlags

import Comic.OCR
import Comic.Translate

import Paths_comictrans(getDataFileName)

defineFlag "lang" ("eng" :: T.Text) "Language of a text we want to OCR"
defineFlag "tesseractCfgPath" ("/home/ikr/radni/tess" :: T.Text) "Path to the Tesseract config"
$(return []) -- workaround for https://github.com/nilcons/hflags/issues/8

data GUI = GUI {
  win :: Window,
  source :: Image,
  input :: Entry,
  translated :: Entry}

yandexApiKey = "trnsl.1.1.20141206T232937Z.afa78ec902bc2385.64360501ae9af320dd9d69ccb190b091299abc2f"

-- | Start GUI.
main :: IO ()
main =
    do $initHFlags "Desktop app for realtime translation of web comics"
       initGUI
       -- Load the GUI from the XML file
       builder <- builderNew
       guiXmlFilePath <- getDataFileName "gui/comic-translate.xml"
       guiXml <- readFile guiXmlFilePath
       builderAddFromString builder $ T.decodeUtf8 guiXml
       -- bindGuiEvents gui (YandexClient yandexApiKey)

       window <- builderGetObject builder castToWindow ("translatorWin" :: String)
       on window deleteEvent $ liftIO (mainQuit >> return False)

       widgetShowAll window
       mainGUI

buildGUI :: Builder -> IO GUI
buildGUI builder = return GUI {
      win        = getWidget castToWindow "translatorWin"
    , source     = getWidget castToImage "sourceImg"
    , input      = getWidget castToEntry "extractedText"
    , translated = getWidget castToEntry "translatedText"
}
  where
    getWidget :: (GObjectClass o) => (obj -> Window) -> String
    getWidget = builderGetObject builder

bindGuiEvents :: Translator a => GUI -> a -> IO HandlerId
bindGuiEvents gui translator =
    do Just screen <- screenGetDefault
       window <- screenGetRootWindow screen
       timeoutAdd (captureScreenshot gui) 25
       timeoutAdd (translateText gui translator) 1000

-- | Helper for taking screenshot in every time interval.
captureScreenshot :: GUI -> IO Bool
captureScreenshot gui =
    do setSourceImage gui
       return True

-- | Take screenshot and set it in the image GUI widget.
setSourceImage :: GUI -> IO ()
setSourceImage gui = screenShot gui >>= imageSetFromPixbuf (source gui)

-- | Ensure value is between given limits (mn and mx)
ensureLimits :: Ord a => a -> a -> a -> a
ensureLimits x mn mx = min mx (max x mn)

-- | Compute correct size from origin, given size and maximum size.
computeSize :: (Num a, Ord a) => a -> a -> a -> a
computeSize o s m = if o + s <= m then s else m - o

overlap :: (Num a, Ord a) => a -> a -> a -> a -> Bool
overlap x xw o ow = or [and [(x <= o), (x + xw >= o)], and [(x <= o + ow), (x + xw >= o + ow)]]

-- | Take a screenshot of a portion of a screen around the mouse pointer.
screenShot :: GUI -> IO Pixbuf
screenShot gui =
    do -- Get screen size.
       Just screen <- screenGetDefault
       window <- screenGetRootWindow screen
       mw <- drawWindowGetWidth window
       mh <- drawWindowGetHeight window

       -- Get current pointer position.
       (_, x, y, _) <- drawWindowGetPointerPos window
       -- Handle overlap of source rectangle and draw window.
       -- Get widget dimensions.
       ws@(ww, wh) <- widgetGetSizeRequest (source gui)
       -- Get widget origin (coordinates of upper left corner).
       maybeOrigin <- widgetTranslateCoordinates (source gui) window 0 0
       let (wx, wy) = fromMaybe (0, 0) maybeOrigin
       --(wx, wy) <- drawWindowGetOrigin (source gui)
       -- Compute screenshot origin and size so that
       -- mouse pointer is in the middle.
       let origin@(ox, oy) = (ensureLimits (x - ww `div` 2) 0 mw,
                              ensureLimits (y - wh `div` 2) 0 mh)
           size = (computeSize ox ww mw, computeSize oy wh mh)

--        Just pxbuf <- pixbufGetFromDrawable window
--            ((uncurry . uncurry Rectangle) origin size)
       pxbuf <- pixbufNew ColorspaceRgb True  0 255 0

       if and [overlap wx ww ox ww, overlap wy wh oy wh]
         then
           do imgPxbf <- imageToPixbuf (source gui)
              case imgPxbf of
                Just px -> return px
                Nothing -> return pxbuf
         else return pxbuf

-- | Take OCR'd text from GUI and translate it by using translation web service.
translateText :: Translator a => GUI -> a -> IO Bool
translateText gui translator =
    do t <- ocrGuiImage (source gui)
       set (input gui) [ entryText := T.unpack t ]
       transText <- translate translator "en" "de" $ T.unpack t
       set (translated gui) [ entryText := T.unpack transText ]
       return True

imageToPixbuf :: Image -> IO (Maybe Pixbuf)
imageToPixbuf img =
    do imageType <- get img imageStorageType
       if imageType == ImagePixbuf
         then
           do pxbf <- imageGetPixbuf img
              return $ Just pxbf
         else return Nothing

pixBufToByteString :: Pixbuf -> IO [Word8]
pixBufToByteString pixbuf =
    do pbd <- (pixbufGetPixels pixbuf :: IO (PixbufData Int Word8))
       arr <- getElems pbd
       return arr

ocrGuiImage :: Image -> IO Text
ocrGuiImage img =
   do imgPxbf <- imageToPixbuf img
      case imgPxbf of
        Just px ->
          -- TODO(ikr): Don't write stuff to disk, do everything in-memory.
          do pixbufSave px "4590temporary39403image39405path39403.png" "png" []
             ocrStoredImage "4590temporary39403image39405path39403.png"
        Nothing -> return $ T.pack "-"

-- | OCR image which is stored on the disk.
ocrStoredImage :: String -> IO Text
ocrStoredImage imgPath =
    do imgBS <- readFile imgPath
       ocrImage imgBS flags_tesseractCfgPath flags_lang >>= return

-- TODO(ikr): Use this instead of writing file to disk
--ocrPixBuf :: Pixbuf -> IO Text
--ocrPixBuf pixbuf =
--    do img <- pixBufToByteString pixbuf
--       text <- ocrImage $ pack img
--       return text
