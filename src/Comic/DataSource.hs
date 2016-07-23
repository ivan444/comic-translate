{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances #-}
-- QSem was deprecated in 7.6, but no more
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module DataSource where

module FB.DataSource
  ( FacebookReq(..)
  , initGlobalState
  , Credentials(..)
  , UserAccessToken
  , AccessToken(..)
  ) where

import Network.HTTP.Conduit
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Typeable
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Conduit
import Data.Conduit.List hiding (mapM, mapM_)
import Data.Monoid
import Data.Aeson
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception

import Haxl.Core

data TranslationParameters = TranslationParameters String String String
data TranslationReq a where
   GetTranslation :: TranslationParameters -> TranslationReq String
  deriving Typeable

deriving instance Eq (TranslationParameters)
deriving instance Eq (TranslationReq a)
deriving instance Show (TranslationParameters)
deriving instance Show (TranslationReq a)
instance Show1 TranslationReq where show1 = show

instance Hashable TranslationParameters where
  hashWithSalt s (TranslationParameters f t source) = hashWithSalt s (f, t ,source)

instance Hashable (TranslationReq a) where
  hashWithSalt s (GetTranslation p) = hashWithSalt s (0::Int,p)
  

instance StateKey TranslationReq where
  data State TranslationReq =
    TranslationState 
       { userAccessToken :: String
       , manager :: Manager
       , numThreads :: Int
       }

instance DataSourceName TranslationReq where
  dataSourceName _ = "Translation"

instance DataSource u TranslationReq where
  fetch = translationFetch

initGlobalState
  :: Int
  -> String
  -> IO (State TranslationReq)
initGlobalState threads token = do
  manager <- newManager tlsManagerSettings
  return TranslationState
    { manager = manager
    , userAccessToken = token
    , numThreads = threads
    }

translationFetch
  :: State TranslationReq
  -> Flags
  -> u
  -> [BlockedFetch TranslationReq]
  -> PerformFetch
translationFetch TranslationState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync manager userAccessToken sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync
  :: Manager -> String -> QSem
  -> BlockedFetch TranslationReq
  -> IO (Async ())
fetchAsync manager tok sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ putSuccess rvar

  -- do
    --e <- Control.Exception.try $
      --     runResourceT $ runTranslationT manager $ fetchReq tok req
    --case e of
      --Left ex -> putFailure rvar (ex :: SomeException)
      --Right a -> putSuccess rvar a


--fetchReq
--  :: String
--  -> TranslationReq a
--  -> FacebookT Auth (ResourceT IO) a

--fetchReq tok (GetObject (Id id)) = error "Undefined"
--  -- should use dataFetch from 
 -- --getObject ("/" <> id) [] (Just tok)


