module Ocr where

import Text.OCR.Tesseract
import Data.ByteString
import Data.Text

-- | Read written text from an image.
ocrImage :: ByteString -> IO Text
ocrImage imgBS =
    do let config = TesseractConfig (Just "/tmp/tess") "eng"
       res <- withTesseractConfig config $ withImageBS imgBS getRecognizedText
       return res
