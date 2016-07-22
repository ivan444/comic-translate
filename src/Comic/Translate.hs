{-# LANGUAGE OverloadedStrings #-}

module Comic.Translate where

import Control.Exception as E
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (_Array, key)
import Data.Text (Text)
import Data.Vector ((!))
import Network.HTTP.Base (urlEncodeVars)
import Network.Wreq (get, responseBody)
import Network.HTTP.Client (HttpException)

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
  (do let translateParams = urlEncodeVars [("key", apiKey), ("lang", sourceLang ++ "-" ++ destLang), ("text", text)]
      r <- get $ "https://translate.yandex.net/api/v1.5/tr.json/translate?" ++ translateParams
      let textVec = r ^. responseBody . key "text" . _Array
      return $ unpackString $ textVec ! 0
  ) `E.catch` handler
  where unpackString (String s) = s
        handler :: HttpException -> IO Text
        handler _ = return ""
