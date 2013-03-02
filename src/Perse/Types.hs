{-# LANGUAGE MultiParamTypeClasses #-}
module Perse.Types where

import Control.Monads
import Database.PostgreSQL.Simple (ConnectInfo)
import Network.Mail.Mime (Address)
import Snap.App.Types

-- | Site-wide configuration.
data Config = Config
  { configPostgres        :: ConnectInfo
  , configDomain          :: String
  , configAdmin           :: Address
  , configSiteAddy        :: Address
  , configCacheDir        :: FilePath
  }

instance AppConfig Config where
  getConfigDomain = configDomain

data PState = PState

-- | Statistics.
data Stats = Stats
  { stEventCount :: Integer
  } deriving Show

instance AppLiftModel Config PState where
  liftModel action = do
    conn <- env controllerStateConn
    anns <- env controllerState
    conf <- env controllerStateConfig
    let state = ModelState conn anns conf
    io $ runReaderT (runModel action) state
