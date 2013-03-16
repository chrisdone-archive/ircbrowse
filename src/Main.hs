{-# LANGUAGE ViewPatterns #-}
-- | Main entry point.

module Main where

import Ircbrowse.Config
import Ircbrowse.Controllers.Cache
import Ircbrowse.Model.Data
import Ircbrowse.Model.Social
import Ircbrowse.Model.Migrations
import Ircbrowse.Server
import Ircbrowse.Types

import Data.Maybe
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
    _ -> do
      db $ migrate False versions
      clearCache config
      runServer config pool
