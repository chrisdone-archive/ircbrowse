{-# LANGUAGE MultiParamTypeClasses #-}
module Perse.Types where

import Control.Monads
import Data.Time
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
  , stMsgCount   :: Integer
  , stNickCount  :: Integer
  , stActiveTimes :: [(Integer,Integer)]
  , stDailyAcitivty :: [(Integer,Integer)]
  , stActiveNicks :: [(String,Integer)]
  , stNetworks :: [String]
  , stChannels :: [String]
  } deriving Show

instance AppLiftModel Config PState where
  liftModel action = do
    conn <- env controllerStateConn
    anns <- env controllerState
    conf <- env controllerStateConfig
    let state = ModelState conn anns conf
    io $ runReaderT (runModel action) state

data Range = Range
  { rangeFrom :: Day, rangeTo :: Day }
  deriving (Eq,Show)

data Key =
  Home Range
  deriving (Eq)
