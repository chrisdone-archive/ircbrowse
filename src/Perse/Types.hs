module Perse.Types where

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
