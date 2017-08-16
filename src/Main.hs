{-# LANGUAGE ViewPatterns #-}
-- | Main entry point.

module Main where

import Data.List
import Data.Maybe
import Ircbrowse.Config
import Ircbrowse.Import
import Ircbrowse.Model.Data
import Ircbrowse.Model.Migrations
import Ircbrowse.Server
import Ircbrowse.Types

import Snap.App
import Snap.App.Cache
import Snap.App.Migrate
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  cpath:action <- getArgs
  config <- getConfig cpath
  pool <- newPool (configPostgres config)
  let db = runDB () config pool
  case foldr const "" action of
    "complete-import" ->
      importRecent False config pool Nothing
    "fast-import" ->
      importRecent True config pool (listToMaybe (drop 1 action))
    "generate-data" -> do
      db $ migrate False versions
      db $ generateData
      clearCache config
    _ -> do
      db $ migrate False versions
      runServer config pool
