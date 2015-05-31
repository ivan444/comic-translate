{-# LANGUAGE OverloadedStrings #-}

module TranslateAPI where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (_Array, key)
import Data.Text
import Data.Vector ((!))
import Network.Wreq as W
import Text.Format (format)

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
  do r <- W.get $ format "https://translate.yandex.net/api/v1.5/tr.json/translate?key={0}&lang={1}-{2}&text={3}" [apiKey, sourceLang, destLang, text]
     let textVec = r ^. responseBody . key "text" . _Array
     return $ unpackString $ textVec ! 0

unpackString (String s) = s
