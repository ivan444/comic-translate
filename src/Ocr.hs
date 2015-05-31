module Ocr where

import Data.ByteString hiding (pack)
import Data.Text
import Text.OCR.Tesseract

-- | Read written text from an image.
ocrImage :: ByteString -> String -> String -> IO Text
ocrImage imgBS tesseractCfgPath lang =
    do let config = TesseractConfig (Just tesseractCfgPath) lang
       res <- withTesseractConfig config $ withImageBS imgBS getRecognizedText
       return $ cleanText res

cleanText :: Text -> Text
cleanText = replace (pack "\n") (pack " ") 
