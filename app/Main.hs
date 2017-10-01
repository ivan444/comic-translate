{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Comic.OCR
import Comic.Pixbuf
import Comic.Translate
import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk  -- TODO: make it qualified or list the used functions
import HFlags (defineFlag, initHFlags)
import Paths_comictrans (getDataFileName)
import Prelude hiding (readFile)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
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
    timeoutAdd (setSourceImage gui >> return True) 25
    timeoutAdd (translateText gui translator) 1000

-- | Take a screenshot and set it in the image GUI widget.
setSourceImage
    :: GUI -> IO ()
setSourceImage gui = screenShot gui >>= imageSetFromPixbuf (source gui)

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
  where
    -- | Returns the adjusted value of 'x' with a guarantee that it fits
    -- between 'mn' and 'mx'.
    ensureLimits
        :: Ord a
        => a -> a -> a -> a
    ensureLimits x mn mx = min mx (max x mn)
    -- | Returns the adjusted size from the origin (based on the given size and
    -- the maximum size).
    computeSize
        :: (Num a, Ord a)
        => a -> a -> a -> a
    computeSize o s m = 
        if o + s <= m
            then s
            else m - o
    -- | Return true if two line segments overlap (if [x, xw] and [o, ow]
    -- overlap). TODO: wrap the line segments in a type!
    overlap
        :: (Num a, Ord a)
        => a -> a -> a -> a -> Bool
    overlap x xw o ow = 
        or
            [ and [(x <= o), (x + xw >= o)]
            , and [(x <= o + ow), (x + xw >= o + ow)]]

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

ocrGuiImage :: Image -> IO T.Text
ocrGuiImage img = do
    imgPxbf <- imageToPixbuf img
    case imgPxbf of
        Just px -> do
            imgContent <- pixbufBytes px
            ocrImage imgContent flags_tesseract_data_dir flags_ocr_lang
        Nothing -> return $ T.pack "-"
