{-# LANGUAGE OverloadedStrings #-}

module TranslateAPI where

import Network.Wreq as W
import Control.Lens
import Data.Aeson.Lens (_Array, key)
import Data.Aeson
import qualified Data.Vector as V
import Data.Text

type SourceLanguage = String
type DestinationLanguage = String
type OriginalText = String

class Translator a where
    -- | Translate text from the source language to the destination language.
    translate :: a -> SourceLanguage -> DestinationLanguage -> OriginalText -> IO Text

data YandexClient = YandexClient { apiKey :: String }  

instance Translator YandexClient where
  translate translator = yandexTranslate apiKey
      where (YandexClient apiKey) = translator

-- | Translate text from the source language to the destination language using Yandex web service. 
yandexTranslate apiKey sourceLang destLang text =
  -- TODO(ikr): Use some formating library to build this string
  do r <- W.get $ "https://translate.yandex.net/api/v1.5/tr.json/translate?key=" ++ apiKey ++ "&lang=" ++ sourceLang ++ "-" ++ destLang ++ "&text=" ++ text
     let textVec = r ^. responseBody . key "text" . _Array
     return $ unpackString $ textVec V.! 0

unpackString (String s) = s
