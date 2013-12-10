{-# LANGUAGE OverloadedStrings #-}

module Ircbrowse.Controllers where

import           Ircbrowse.Data
import           Ircbrowse.Model.Events
import           Ircbrowse.Model.Stats
import           Ircbrowse.Model.Social
import           Ircbrowse.Model.Nicks
import           Ircbrowse.Model.Profile
import           Ircbrowse.Model.Quotes
import           Ircbrowse.Monads
import           Ircbrowse.Types
import           Ircbrowse.Types.Import
import           Ircbrowse.View.Browse as V
import           Ircbrowse.View.NickCloud as V
import           Ircbrowse.View.Overview as V
import           Ircbrowse.View.Social as V
import           Ircbrowse.View.Calendar as V
import           Ircbrowse.View.Profile as V
import           Ircbrowse.View.Nicks as V

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Safe
import           Snap.App
import           Snap.App.RSS
import           Snap.App.Cache
import           System.Locale
import           Text.Blaze.Pagination

--------------------------------------------------------------------------------
-- Controllers

overview :: Controller Config PState ()
overview = do
  viewCached Overview $ do
    return $ V.overview

stats :: Controller Config PState ()
stats = do
  range <- getRange
  channel <- getChannel
  viewCached (StatsOverview channel range) $ do
    stats <- model $ getStats channel range
    return $ V.stats channel range stats

channel :: Controller Config PState ()
channel = do
  channel <- getChannel
  viewCached (Channel channel) $ do
    return $ V.channel channel

calendar :: Controller Config PState ()
calendar = do
  channel <- getChannel
  viewCached (Calendar channel) $ do
    day <- model $ getFirstEventDate channel
    today <- fmap utctDay (io getCurrentTime)
    return $ V.calendar day today channel

nickProfile :: Controller Config PState ()
nickProfile = do
  nick <- getText "nick"
  recent <- getBoolean "recent" True
  range <- getRange
  viewCached (Profile nick recent range) $ do
    hours <- model $ activeHours nick recent range
    return $ V.nickProfile nick recent hours

allNicks :: Controller Config PState ()
allNicks = do
  recent <- getBoolean "recent" True
  range <- getRange
  channel <- getChannel
  viewCached (AllNicks recent range) $ do
    nicks <- model $ getNicks channel 100 recent range
    count <- model $ getNickCount channel recent range
    return $ V.nicks channel count nicks recent

socialGraph :: Controller Config PState ()
socialGraph = do
  range <- getRange
  channel <- getChannelMaybe
  viewCached (Social channel range) $ do
    graph <- model $ getSocialGraph channel range
    return $ V.socialGraph graph

nickCloud :: Controller Config PState ()
nickCloud = do
  range <- getRange
  channel <- getChannel
  viewCached (NickCloud channel range) $ do
    nicks <- model $ getNickStats channel range
    return $ V.nickCloud nicks

browseDay :: Bool -> Controller Config PState ()
browseDay today =
  if today
    then do today <- io (fmap utctDay getCurrentTime)
            browseSomeDay today (T.pack (formatTime defaultTimeLocale "%Y/%m/%d" today))
    else browseGivenDate

browseGivenDate = do
  year <- getText "year"
  month <- getText "month"
  day <- getText "day"
  let datetext = year <> "/" <> month <> "/" <> day
  case parseTime defaultTimeLocale "%Y/%m/%d" (T.unpack datetext) of
    Nothing -> return ()
    Just day -> browseSomeDay day datetext

browseSomeDay :: Day -> Text -> Controller Config PState ()
browseSomeDay day datetext = do
  evid <- getIntegerMaybe "id"
  timestamp <- getTimestamp
  channel <- getChannel
  mode <- fmap (fromMaybe "everything") (getTextMaybe "mode")
  out <- cache (BrowseDay channel day mode) $ do
    events <- model $ getEventsByDay channel day (mode == "everything")
    uri <- getMyURI
    return $ Just $ V.browseDay channel mode datetext events uri
  maybe (return ()) outputText out

browse :: Controller Config PState ()
browse = do
  evid <- getIntegerMaybe "id"
  timestamp <- getTimestamp
  channel <- getChannel
  q <- getSearchText "q"
  pn <- getPagination "events"
  let pn' = pn { pnResultsPerPage = Just [25,35,50,100] }
  out <- cacheIf (isNothing q) (Browse channel evid pn') $ do
    (pagination,logs) <- model $ getEvents channel evid pn' q
    uri <- getMyURI
    return $ Just $ V.browse channel uri timestamp logs pn' { pnPn = pagination } q
  maybe (return ()) outputText out

pdfs :: Controller Config PState ()
pdfs = do
  unique <- fmap isJust (getTextMaybe "unique")
  if unique
     then uniquePdfs
     else paginatedPdfs

uniquePdfs = do
  channel <- getChannel
  uri <- getMyURI
  viewCached (UniquePDFs channel) $ do
    pdfs <- model $ getAllPdfs channel
    return $ V.allPdfs uri channel pdfs

paginatedPdfs = do
  channel <- getChannel
  timestamp <- getTimestamp
  pn <- getPagination "events"
  let pn' = pn { pnResultsPerPage = Just [25,35,50,100] }
  viewCached (PDFs channel pn') $ do
    (pagination,logs) <- model $ getPaginatedPdfs channel pn'
    uri <- getMyURI
    return $ V.pdfs channel uri timestamp logs pn' { pnPn = pagination } Nothing

quotes :: Controller Config PState ()
quotes = do
  return ()
  qs <- model $ getRecentQuotes 30
  outputRSS "IRCBrowse Quotes"
            "http://ircbrowse.net/quotes.rss"
            (map (\(eid,date,title) -> (date,title,"",T.pack (makeLink eid date)))
                 qs)

  where makeLink eid t =
          concat ["http://ircbrowse.net/browse/haskell?id="
                 ,show eid
                 ,"&timestamp="
                 ,secs
                 ,"#t"
                 ,secs]
         where secs = formatTime defaultTimeLocale "%s" t

--------------------------------------------------------------------------------
-- Utilities

getRange :: Controller c s Range
getRange = do
  now <- io getCurrentTime
  let range = Range (addDays (-31) (utctDay now)) (utctDay now)
  return range

getChannel :: Controller c s Channel
getChannel = getChannelMaybe
             >>= maybe (error "expected a channel on this page!")
                       return

getChannelMaybe :: Controller c s (Maybe Channel)
getChannelMaybe = do
  chan <- getStringMaybe "channel"
  return $ chan >>= parseChan

getTimestamp :: Controller c s (Maybe UTCTime)
getTimestamp = do
  string <- getStringMaybe "timestamp"
  return $ string >>= parseTime defaultTimeLocale "%s"

getSearchText :: ByteString -> Controller c s (Maybe Text)
getSearchText key = do
  v <- getTextMaybe key
  case fmap (T.filter (not.isSpace)) v of
    Nothing -> return Nothing
    Just e | T.null e -> return Nothing
           | otherwise -> return v

-- | Get text (maybe).
getTextMaybe :: ByteString -> Controller c s (Maybe Text)
getTextMaybe name = do
  pid <- fmap (fmap T.decodeUtf8) (getParam name)
  return pid

-- | Get text (maybe).
getText :: ByteString -> Controller c s Text
getText name = do
  getTextMaybe name >>= maybe (error ("expected param: " ++ show name)) return

-- | Get integer parmater.
getIntegerMaybe :: ByteString -> Controller c s (Maybe Integer)
getIntegerMaybe name = do
  pid <- fmap (>>= readMay) (getStringMaybe name)
  return pid

-- | Get a boolean value.
getBoolean :: ByteString -> Bool -> Controller c s Bool
getBoolean key def = fmap (maybe def (=="true")) (getTextMaybe key)
