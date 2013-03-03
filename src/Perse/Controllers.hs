{-# LANGUAGE OverloadedStrings #-}

module Perse.Controllers where

import Perse.Controllers.Cache
import Perse.Data
import Perse.Model.Stats
import Perse.Model.Events
import Perse.Monads
import Perse.Types
import Perse.View.Browse as V
import Perse.View.Overview as V

import Data.ByteString (ByteString)
import Snap.App
import System.Locale

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
  timestamp <- getTimestamp
  network <- getStringMaybe "network"
  channel <- getStringMaybe "channel"
  q <- getSearchString "q"
  pagination <- getPagination
  out <- cacheIf (isNothing q) (Browse network channel timestamp pagination) $ do
    (pagination',total,logs) <- model $ getEvents network channel timestamp pagination q
    let pn = pagination' { pnResults = fromIntegral (length logs)
                         , pnTotal = total
                         }
    uri <- getMyURI
    return $ Just $ V.browse uri network channel timestamp logs pn q
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

getSearchString :: ByteString -> Controller c s (Maybe String)
getSearchString key = do
  v <- getStringMaybe key
  case fmap trim v of
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just v  -> return (Just v)
