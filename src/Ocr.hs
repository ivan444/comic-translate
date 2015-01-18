module Ocr where

import Text.OCR.Tesseract
import Data.ByteString
--import Data.Text


-- | Read written text from an image.
ocrImage :: ByteString -> IO String
ocrImage imgBS =
    do let config = TesseractConfig tessdataDir "eng"
       res <- withTesseractConfig config $ withImageBS imgBS getRecognizedText
       return "bla"
       --checkRecognizedText [res] image1TextLines
