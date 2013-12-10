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
  = StatsOverview Channel Range
  | Overview
  | NickCloud Channel Range
  | Social (Maybe Channel) Range
  | Browse Channel (Maybe Integer) PN
  | BrowseDay Channel Day Text
  | Profile Text Bool Range
  | AllNicks Bool Range
  | PDFs Channel PN
  | UniquePDFs Channel
  | Calendar Channel
  | Channel Channel

instance Key CacheKey where
  keyToString (Calendar channel) = "calendar-" ++ showChan channel ++ ".html"
  keyToString (BrowseDay channel day mode) = "browse-day-" ++ showDay day ++ "-" ++ showChan channel ++ "-" ++ unpack mode ++ ".html"
  keyToString (UniquePDFs channel) = "unique-pdfs-" ++ showChan channel ++ ".html"
  keyToString (StatsOverview channel range) = contexted "overview" (Just channel) range
  keyToString Overview = "overview"
  keyToString (NickCloud channel range) = contexted "nick-cloud" (Just channel) range
  keyToString (Social channel range) = contexted "social" channel range
  keyToString (Browse channel evid (PN _ pagination _)) =
    "browse-" ++ opt (Just (showChan channel)) ++ maybe "" (("-"++).show) evid ++
    "-page" ++ show (pnCurrentPage pagination) ++ "-of-" ++ show (pnPerPage pagination) ++ ".html"
      where opt Nothing = "_"
            opt (Just x) = x
  keyToString (Profile nick recent (Range from to)) =
    "profile-" ++ unpack nick ++ "-" ++ (if recent then "recent-" else "all-") ++
    showDay from ++ "-" ++ showDay to ++ ".html"
  keyToString (AllNicks recent (Range from to)) =
    "nicks-" ++ "-" ++ (if recent then "recent-" else "all-") ++
    showDay from ++ "-" ++ showDay to ++ ".html"
  keyToString (PDFs channel (PN _ pagination _)) =
    "pdfs-" ++ opt (Just (showChan channel)) ++
    "-page" ++ show (pnCurrentPage pagination) ++ "-of-" ++ show (pnPerPage pagination) ++ ".html"
      where opt Nothing = "_"
            opt (Just x) = x
  keyToString (Channel channel) = "channel-" ++ showChan channel ++ ".html"

contexted name channel (Range from to) =
  name ++ "-" ++ opt (fmap showChan channel) ++ "-" ++ showDay from ++ "-" ++ showDay to ++ ".html"
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
