{-# LANGUAGE ViewPatterns #-}
-- | Main entry point.

module Main where

import Ircbrowse.Config
import Ircbrowse.Controllers.Cache
import Ircbrowse.Model.Data
import Ircbrowse.Model.Migrations
import Ircbrowse.Server
import Ircbrowse.Types
import Ircbrowse.Import

import Database.PostgreSQL.Base   (newPool)
import Snap.App
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
    "create-version" -> do
      db $ migrate True versions
    "generate-data" -> do
      db $ generateData
      clearCache config
    "import-yesterday" -> do
      importYesterday config pool
      clearCache config
    _ -> do
      db $ migrate False versions
      clearCache config
      runServer config pool
