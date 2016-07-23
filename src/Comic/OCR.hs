{-# LANGUAGE OverloadedStrings #-}

module Comic.OCR where

import Data.ByteString hiding (pack)
import Data.Text
--import Text.OCR.Tesseract

-- tesseract stdin stdout -l eng --tessdata-dir=$PWD/tessdata -psm 1

-- | Read written text from an image.
ocrImage :: ByteString -> Text -> Text -> IO Text
ocrImage imgBS tesseractCfgPath lang = return "dummy"
--    do let config = TesseractConfig (Just tesseractCfgPath) lang
--       res <- withTesseractConfig config $ withImageBS imgBS getRecognizedText
--       return $ cleanText res

cleanText :: Text -> Text
cleanText = replace (pack "\n") (pack " ") 
