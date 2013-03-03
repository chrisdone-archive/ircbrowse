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

import Snap.App

--------------------------------------------------------------------------------
-- Controllers

overview :: Controller Config PState ()
overview = do
  range <- getRange
  network <- getStringMaybe "network"
  channel <- getStringMaybe "channel"
  out <- cacheIf False (Overview network channel range) $ do
    stats <- model $ getStats network channel range
    return $ Just $ V.overview network channel range stats
  maybe (return ()) outputText out

browse :: Controller Config PState ()
browse = do
  range <- getRange
  network <- getStringMaybe "network"
  channel <- getStringMaybe "channel"
  pagination <- getPagination
  out <- cacheIf False (Browse network channel range) $ do
    (total,logs) <- model $ getEvents network channel range pagination
    let pn = pagination { pnResults = fromIntegral (length logs)
                        , pnTotal = total
                        }
    return $ Just $ V.browse network channel range logs pn
  maybe (return ()) outputText out

--------------------------------------------------------------------------------
-- Utilities

getRange :: Controller c s Range
getRange = do
  now <- io getCurrentTime
  let range = Range (addDays (-31) (utctDay now)) (utctDay now)
  return range
