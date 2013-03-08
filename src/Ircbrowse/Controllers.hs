{-# LANGUAGE OverloadedStrings #-}

module Ircbrowse.Controllers where

import           Ircbrowse.Controllers.Cache
import           Ircbrowse.Data
import           Ircbrowse.Model.Stats
import           Ircbrowse.Model.Events
import           Ircbrowse.Monads
import           Ircbrowse.Types
import           Ircbrowse.View.Browse as V
import           Ircbrowse.View.Overview as V

import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Safe
import           Snap.App
import           System.Locale
import           Text.Blaze.Pagination

--------------------------------------------------------------------------------
-- Controllers

overview :: Controller Config PState ()
overview = do
  range <- getRange
  network <- getStringMaybe "network"
  channel <- getStringMaybe "channel"
  out <- cache (Overview network channel range) $ do
    stats <- model $ getStats network channel range
    return $ Just $ V.overview network channel range stats
  maybe (return ()) outputText out

browse :: Controller Config PState ()
browse = do
  evid <- getIntegerMaybe "id"
  timestamp <- getTimestamp
  network <- getStringMaybe "network"
  channel <- getStringMaybe "channel"
  q <- getSearchText "q"
  pn <- getPagination "events"
  let pn' = pn { pnResultsPerPage = Just [25,35,50,100] }
  out <- cacheIf (isNothing q) (Browse network channel evid pn') $ do
    (pagination,logs) <- model $ getEvents network channel evid pn' q
    uri <- getMyURI
    return $ Just $ V.browse uri network channel timestamp logs pn' { pnPn = pagination } q
  maybe (return ()) outputText out

--------------------------------------------------------------------------------
-- Utilities

getRange :: Controller c s Range
getRange = do
  now <- io getCurrentTime
  let range = Range (addDays (-31) (utctDay now)) (utctDay now)
  return range

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

-- | Get integer parmater.
getIntegerMaybe :: ByteString -> Controller c s (Maybe Integer)
getIntegerMaybe name = do
  pid <- fmap (>>= readMay) (getStringMaybe name)
  return pid
