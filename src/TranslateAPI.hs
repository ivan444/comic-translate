{-# LANGUAGE OverloadedStrings #-}

module TranslateAPI where

import Network.Wreq as W
import Control.Lens
import Data.Aeson.Lens (_Array, key)
import Data.Aeson
import qualified Data.Vector as V

-- TODO(ikr): Make it generic (not dependent on concrete API).

yandexApiKey = "trnsl.1.1.20141206T232937Z.afa78ec902bc2385.64360501ae9af320dd9d69ccb190b091299abc2f"

-- | Translate text from source language to destination language. 
translate apiKey sourceLang destLang text =
  --do r <- W.get $ format "https://translate.yandex.net/api/v1.5/tr.json/translate?key={0}&lang={1}-{2}&text={3}" [apiKey, sourceLang, destLang, text]
  do r <- W.get $ "https://translate.yandex.net/api/v1.5/tr.json/translate?key=" ++ apiKey ++ "&lang=" ++ sourceLang ++ "-" ++ destLang ++ "&text=" ++ text
     let textVec = r ^. responseBody . key "text" . _Array
     return $ unpackString $ textVec V.! 0

unpackString (String s) = s
