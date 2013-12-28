{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ircbrowse.Types where

import Ircbrowse.Data
import Ircbrowse.Types.Import
import Ircbrowse.Monads

import Control.Applicative
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.Mail.Mime (Address)
import Snap.App.Cache
import Snap.App.Types
import System.Locale
import Text.Blaze.Pagination

-- | Site-wide configuration.
data Config = Config
  { configPostgres        :: !ConnectInfo
  , configDomain          :: !String
  , configAdmin           :: !Address
  , configSiteAddy        :: !Address
  , configCacheDir        :: !FilePath
  , configLogDir          :: !FilePath
  }

instance AppConfig Config where
  getConfigDomain = configDomain

instance CacheDir Config where
  getCacheDir = configCacheDir

data PState = PState

-- | Statistics.
data Stats = Stats
  { stEventCount         :: !Integer
  , stMsgCount           :: !Integer
  , stNickCount          :: !Integer
  , stActiveTimes        :: ![(Integer,Integer)]
  , stDailyActivity      :: ![(Integer,Integer)]
  , stActiveNicks        :: ![(String,Integer)]
  , stNetworks           :: ![(String,String)]
  , stChannels           :: ![(String,String)]
  , stActivityByYear     :: ![(Integer,Integer)]
  , stConversationByYear :: ![(Integer,Integer)]
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
  { rangeFrom :: !Day, rangeTo :: !Day }
  deriving (Eq,Show)

data CacheKey
  = StatsOverview Channel
  | Overview
  | NickCloud Channel
  | Social (Maybe Channel)
  | BrowseDay Channel Day Text
  | BrowseToday Channel Text
  | Profile Text Bool
  | AllNicks Channel Bool
  | UniquePDFs Channel
  | Calendar Channel
  | Channel Channel

instance Key CacheKey where
  keyToString (Calendar channel) = "calendar-" ++ showChan channel ++ ".html"
  keyToString (BrowseToday channel mode) = "browse-today-" ++ showChan channel ++ "-" ++ unpack mode ++ ".html"
  keyToString (BrowseDay channel day mode) = "browse-day-" ++ showDay day ++ "-" ++ showChan channel ++ "-" ++ unpack mode ++ ".html"
  keyToString (UniquePDFs channel) = "unique-pdfs-" ++ showChan channel ++ ".html"
  keyToString (StatsOverview channel) = contexted "overview" (Just channel)
  keyToString Overview = "overview.html"
  keyToString (NickCloud channel) = contexted "nick-cloud" (Just channel)
  keyToString (Social channel) = contexted "social" channel
  keyToString (Profile nick recent) =
   "profile-" ++ unpack nick ++ "-" ++ (if recent then "recent" else "all") ++
   ".html"
  keyToString (AllNicks channel recent) =
    "nicks-" ++ showChan channel ++ "-" ++ (if recent then "recent" else "all") ++
    ".html"
  keyToString (Channel channel) = "channel-" ++ showChan channel ++ ".html"

contexted name channel =
  name ++ "-" ++ opt (fmap showChan channel) ++ ".html"
    where opt Nothing = "_"
          opt (Just x) = x

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"

data Event = Event
  { eventId        :: !Int
  , eventTimestamp :: !ZonedTime
  , eventNetwork   :: !Int
  , eventChannel   :: !Int
  , eventType      :: !Text
  , eventNick      :: !(Maybe Text)
  , eventText      :: !Text
  } deriving (Show)

instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field
