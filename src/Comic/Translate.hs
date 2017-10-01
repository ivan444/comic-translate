{-# LANGUAGE OverloadedStrings #-}

module Comic.Translate where

import Control.Exception as E
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (_Array, key)
import qualified Data.Text as T
import Data.Vector ((!))
import Network.HTTP.Base (urlEncodeVars)
import Network.Wreq (get, responseBody)
import Network.HTTP.Client (HttpException)

type SourceLanguage = T.Text

type DestinationLanguage = T.Text

type OriginalText = T.Text

class Translator a  where
    -- | Translate text from the source language to the destination language.
    translate
        :: a
        -> SourceLanguage
        -> DestinationLanguage
        -> OriginalText
        -> IO T.Text

data YandexClient = YandexClient
    { apiKey :: String
    } 

instance Translator YandexClient where
    translate translator = yandexTranslate apiKey
      where
        (YandexClient apiKey) = translator

-- | Translate text from the source language to the destination language using Yandex web service. 
yandexTranslate apiKey sourceLang destLang text = 
    (do let translateParams = 
                urlEncodeVars
                    [ ("key", apiKey)
                    , ( "lang"
                      , (T.unpack sourceLang) ++ "-" ++ (T.unpack destLang))
                    , ("text", T.unpack text)]
        r <- 
            get $ "https://translate.yandex.net/api/v1.5/tr.json/translate?" ++
            translateParams
        let textVec = r ^. responseBody . key "text" . _Array
        return $ unpackString $ textVec ! 0) `E.catch`
    handler
  where
    unpackString (String s) = s
    handler :: HttpException -> IO T.Text
    handler _ = return ""
