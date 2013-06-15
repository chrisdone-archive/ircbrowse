{-# LANGUAGE ViewPatterns #-}
-- | Main entry point.

module Main where

import Ircbrowse.Config
import Ircbrowse.Model.Migrations
import Ircbrowse.Server
import Ircbrowse.Types
import Ircbrowse.ImportTunes

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
    "import-yesterday" ->
      importYesterday config pool
    _ -> do
      db $ migrate False versions
      clearCache config
      runServer config pool
