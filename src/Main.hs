module Main where

import System.Environment
import Control.Monad.Trans (liftIO)

import Data.Word
import Data.Array.MArray
import Data.ByteString.Unsafe
import Data.ByteString
import Foreign.Marshal.Alloc
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Pixbuf  
import Graphics.UI.Gtk.Gdk.Screen

import Ocr

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

-- | Load XML from glade path.
-- Note: crashes with a runtime error on console if fails!
loadGlade gladePath =
    do Just xml <- xmlNew gladePath
       gwin <- xmlGetWidget xml castToWindow "translatorWin"
       gsource <- xmlGetWidget xml castToImage "sourceImg"
       ginput <- xmlGetWidget xml castToEntry "extractedText"
       gtranslated <- xmlGetWidget xml castToLabel "translatedText"
       return $ GUI gwin gsource ginput gtranslated

bindGuiEvents gui =
    do onDestroy (win gui) mainQuit
       on (win gui) configureEvent $ liftIO $ setSourceImage gui >> return False
       timeoutAdd (timedScreenshot gui) 25

-- | Helper for taking screenshot in every time interval.
timedScreenshot gui =
    do setSourceImage gui
       return True

-- | Take screenshot and set it in the image GUI widget.
setSourceImage gui = screenShot gui >>= imageSetFromPixbuf (source gui) 

-- | Ensure value is between given limits (mn and mx)
ensureLimits x mn mx = min mx (max x mn)

-- | Compute correct size from origin, given size and maximum size.
computeSize o s m = if o + s <= m then s else m - o

-- | Take a screenshot of a portion of a screen around the mouse pointer.
screenShot :: GUI -> IO Pixbuf
screenShot gui =
    do -- Get screen size.
       Just screen <- screenGetDefault
       window <- screenGetRootWindow screen
       (mw, mh) <- drawableGetSize window

       -- Get current pointer position.
       (_, x, y, _) <- drawWindowGetPointerPos window

       -- Compute screenshot origin and size so that
       -- mouse pointer is in the middle.
       (ww, wh) <- widgetGetSize (source gui)
       let origin@(ox, oy) = (ensureLimits (x - ww `div` 2) 0 mw,
                              ensureLimits (y - wh `div` 2) 0 mh)
       let size = (computeSize ox ww mw, computeSize oy wh mh)
       Just pxbuf <- pixbufGetFromDrawable window
           ((uncurry . uncurry Rectangle) origin size)
       return pxbuf

pixBufToByteString :: Pixbuf -> IO [Word8]
pixBufToByteString pixbuf =
    do pbd <- (pixbufGetPixels pixbuf :: IO (PixbufData Int Word8))
       --arr <- readArray pbd 0
       arr <- getElems pbd
       return arr

ocrPixBuf :: Pixbuf -> IO String
ocrPixBuf pixbuf =
    do img <- pixBufToByteString pixbuf
       text <- ocrImage $ pack img
       return text
