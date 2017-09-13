{-# LANGUAGE OverloadedStrings #-}

module Comic.OCR
  (ocrImage)
  where

import HSH.Command ((-|-), ShellCommand(..), run)
import HSH.ShellEquivs (echo)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

tesseractCmd :: String
tesseractCmd = "tesseract"

buildTesseractArgs :: String -> String -> [String]
buildTesseractArgs tessdataPath lang =
    [ "stdin"
    , "stdout"
    , "-l " ++ lang
    , "--tessdata-dir '" ++ tessdataPath ++ "'"
    , "-psm 1"]

-- | Read written text from an image.
ocrImage
    :: B.ByteString -> T.Text -> T.Text -> IO T.Text
ocrImage imageContent tessdataPath lang = do
    res <- run $ echo imageContent -|- (tesseractCmd, tesseractArgs)
    return $ cleanText $ T.decodeUtf8 res
  where
    tesseractArgs :: [String]
    tesseractArgs = buildTesseractArgs (T.unpack tessdataPath) (T.unpack lang)
    cleanText :: T.Text -> T.Text
    cleanText = T.replace (T.pack "\n") (T.pack " ")
