module Main where

import System.Environment
import Control.Monad.Trans (liftIO)
import Prelude hiding (readFile)

import Data.Word
import Data.Array.MArray
import Data.ByteString.Unsafe
import Data.ByteString hiding (putStrLn, unpack)
import Data.Text as T
import Foreign.Marshal.Alloc
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Pixbuf  
import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Gdk.EventM

import Ocr
import TranslateAPI

import Paths_rtit(getDataFileName)

data GUI = GUI {
  win :: Window,
  source :: Image,
  input :: Entry,
  translated :: Label}

-- | Start GUI.
main :: IO ()
main =
    do initGUI
       -- Load the GUI from the Glade file
       gladePath <- getDataFileName "gui/rtit.glade"
       gui <- loadGlade gladePath
       bindGuiEvents gui
       mainGUI

-- | Path to the Tesseract config
-- TODO(ikr): Make it a command line argument.
tesseractCfgPath :: String
tesseractCfgPath = "/home/ikr/radni/tess"

-- | Language of a text we want to OCR.
-- TODO(ikr): Make it a command line argument.
lang :: String
lang = "eng"

-- | Load XML from glade path.
-- Note: crashes with a runtime error in console if fails!
loadGlade gladePath =
    do Just xml <- xmlNew gladePath
       gwin <- xmlGetWidget xml castToWindow "translatorWin"
       gsource <- xmlGetWidget xml castToImage "sourceImg"
       ginput <- xmlGetWidget xml castToEntry "extractedText"
       gtranslated <- xmlGetWidget xml castToLabel "translatedText"
       return $ GUI gwin gsource ginput gtranslated

bindGuiEvents gui =
    do onDestroy (win gui) mainQuit
       Just screen <- screenGetDefault
       window <- screenGetRootWindow screen
       timeoutAdd (captureScreenshot gui) 25
       timeoutAdd (translateText gui) 3000

-- | Helper for taking screenshot in every time interval.
captureScreenshot gui =
    do setSourceImage gui
       return True

-- | Take screenshot and set it in the image GUI widget.
setSourceImage gui = screenShot gui >>= imageSetFromPixbuf (source gui) 

-- | Ensure value is between given limits (mn and mx)
ensureLimits x mn mx = min mx (max x mn)

-- | Compute correct size from origin, given size and maximum size.
computeSize o s m = if o + s <= m then s else m - o

overlap x xw o ow = or [and [(x <= o), (x + xw >= o)], and [(x <= o + ow), (x + xw >= o + ow)]]

-- | Take a screenshot of a portion of a screen around the mouse pointer.
screenShot :: GUI -> IO Pixbuf
screenShot gui =
    do -- Get screen size.
       Just screen <- screenGetDefault
       window <- screenGetRootWindow screen
       (mw, mh) <- drawableGetSize window

       -- Get current pointer position.
       (_, x, y, _) <- drawWindowGetPointerPos window
       -- Handle overlap of source rectangle and draw window.
       -- Get widget dimensions.
       ws@(ww, wh) <- widgetGetSize (source gui)
       -- Get widget origin (coordinates of upper left corner).
       (wx, wy) <- widgetGetDrawWindow (source gui) >>= drawWindowGetOrigin
       -- Compute screenshot origin and size so that
       -- mouse pointer is in the middle.
       let origin@(ox, oy) = (ensureLimits (x - ww `div` 2) 0 mw,
                              ensureLimits (y - wh `div` 2) 0 mh)
           size = (computeSize ox ww mw, computeSize oy wh mh)

       Just pxbuf <- pixbufGetFromDrawable window
           ((uncurry . uncurry Rectangle) origin size)

       if and [overlap wx ww ox ww, overlap wy wh oy wh]
         then
           do imgPxbf <- imageToPixbuf (source gui)
              case imgPxbf of
                Just px -> return px
                Nothing -> return pxbuf
         else return pxbuf

-- | Take OCR'd text from GUI and translate it by using translation web service.
translateText :: GUI -> IO Bool
translateText gui =
    do t <- ocrGuiImage (source gui)
       transText <- translate yandexApiKey "en" "de" (T.unpack t)
       set (translated gui) [ labelText := "_" ++ (show transText) ++ "_" ]
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
          -- TODO(ikr): Don't write stuff to disk, make everything in-memory.
          do pixbufSave px "4590temporary39403image39405path39403.png" "png" []
             ocrStoredImage "4590temporary39403image39405path39403.png"
        Nothing -> return $ T.pack "-"

-- | OCR image which is stored on the disk.
-- TODO(ikr): Make it accept Pixbuf directly. Skip the disk.
ocrStoredImage :: String -> IO Text
ocrStoredImage imgPath =
    do imgBS <- readFile imgPath
       ocrImage imgBS tesseractCfgPath lang >>= return

--ocrPixBuf :: Pixbuf -> IO Text
--ocrPixBuf pixbuf =
--    do img <- pixBufToByteString pixbuf
--       text <- ocrImage $ pack img
--       return text
