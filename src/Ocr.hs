module Ocr where

import Text.OCR.Tesseract
import Data.ByteString hiding (pack)
import Data.Text

-- | Read written text from an image.
ocrImage :: ByteString -> IO Text
ocrImage imgBS =
    do let config = TesseractConfig (Just "/home/ikr/radni/tess") "eng"
       res <- withTesseractConfig config $ withImageBS imgBS getRecognizedText
       return $ cleanText res

cleanText :: Text -> Text
cleanText = replace (pack "\n") (pack " ") 
