{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ircbrowse.Types where

import Ircbrowse.Data
import Ircbrowse.Monads

import Data.Text
import Database.PostgreSQL.Simple (ConnectInfo)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))
import Network.Mail.Mime (Address)
import Snap.App.Types
import Text.Blaze.Pagination

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
  , stDailyActivity :: [(Integer,Integer)]
  , stActiveNicks :: [(String,Integer)]
  , stNetworks :: [(String,String)]
  , stChannels :: [(String,String)]
  , stActivityByYear :: [(Integer,Integer)]
  , stConversationByYear :: [(Integer,Integer)]
  } deriving Show

instance Default Stats where
  def = Stats
    { stEventCount = 0
    , stMsgCount   = 0
    , stNickCount = 0
    , stActiveNicks = []
    , stActiveTimes = []
    , stDailyActivity = []
    , stNetworks = []
    , stChannels = []
    , stConversationByYear = []
    , stActivityByYear = []
    }

instance AppLiftModel Config PState where
  liftModel action = do
    conn <- env controllerStateConn
    anns <- env controllerState
    conf <- env controllerStateConfig
    let st = ModelState conn anns conf
    io $ runReaderT (runModel action) st

data Range = Range
  { rangeFrom :: Day, rangeTo :: Day }
  deriving (Eq,Show)

data Key
  = Overview (Maybe String) (Maybe String) Range
  | NickCloud (Maybe String) (Maybe String) Range
  | Social (Maybe String) (Maybe String) Range
  | Browse (Maybe String) (Maybe String) (Maybe Integer) PN

data Event = Event
  { eventId        :: !Int
  , eventTimestamp :: !ZonedTime
  , eventNetwork   :: !Int
  , eventChannel   :: !Int
  , eventType      :: !Text
  , eventNick      :: !(Maybe Text)
  , eventText      :: !Text
  }

instance QueryResults Event where
  convertResults field values = Event
    { eventId = _1
    , eventTimestamp = _2
    , eventNetwork = _3
    , eventChannel = _4
    , eventType = _5
    , eventNick = _6
    , eventText = _7
    }
    where (_1,_2,_3,_4,_5,_6,_7) = convertResults field values
