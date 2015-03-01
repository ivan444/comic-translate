module Main where

import System.Environment
import Control.Monad.Trans (liftIO)
import Prelude hiding (readFile)

import Data.Word
import Data.Array.MArray
import Data.ByteString.Unsafe
import Data.ByteString hiding (putStrLn, unpack)
import Data.Text as T hiding (pack)
import Foreign.Marshal.Alloc
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Pixbuf  
import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Gdk.EventM

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
       Just screen <- screenGetDefault
       window <- screenGetRootWindow screen
       --windowPropagateKeyEvent window $ do
       --   [Control] <- eventModifier
       --   "Return" <- eventKeyName
       --   return 
       --dw <- widgetGetDrawWindow (win gui)
       --_ <- pointerGrab dw False [AllEventsMask] (Nothing :: Maybe DrawWindow) Nothing currentTime
       --on (win gui) keyPressEvent $ tryEvent $ do
       --   [Control] <- eventModifier
       --   "Return" <- eventKeyName
       --   liftIO $ putStrLn "Ctrl-Return pressed"
       --on (win gui) configureEvent $ liftIO $ setSourceImage gui >> return False
       timeoutAdd (timedScreenshot gui) 25
       timeoutAdd (timedTranslate gui) 3000

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
         then imageGetPixbuf (source gui) >>= return
         else return pxbuf


timedTranslate gui =
    do t <- ocrGuiImage (source gui)
       set (translated gui) [ labelText := "_" ++ (show t) ++ "_" ]
       return True

pixBufToByteString :: Pixbuf -> IO [Word8]
pixBufToByteString pixbuf =
    do pbd <- (pixbufGetPixels pixbuf :: IO (PixbufData Int Word8))
       arr <- getElems pbd
       return arr

ocrPixBuf :: Pixbuf -> IO Text
ocrPixBuf pixbuf =
    do img <- pixBufToByteString pixbuf
       text <- ocrImage $ pack img
       return text

ocrGuiImage img =
    do pxbf <- imageGetPixbuf img
       imageType <- get img imageStorageType
       pixbufSave pxbf "4590temporary39403image39405path39403.png" "png" []
       locr "4590temporary39403image39405path39403.png"
       --if imageType == ImagePixbuf then (return $ locr "4590temporary39403image39405path39403.png") else ("No img"::String) >>= return

locr p =
    do imgBS <- readFile p
       ocrImage imgBS >>= return

--ocrGuiImage2 :: Image -> IO [Char]
--ocrGuiImage2 img =
--    do pxbf <- imageGetPixbuf img
--       imageType <- get pxbf imageStorageType
--       --if imageType == ImagePixbuf
--       --  then
--       --    imageGetPixbuf img >>= ocrPixBuf >>= return
--         --then do
--         --  pxb <- imageGetPixbuf img
--         --  return $ ocrPixBuf pxb
--       if imageType == ImagePixbuf then (imageGetPixbuf img >>= ocrPixBuf >>= unpack >>= return) else return "No img"
