{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The web server.

module Ircbrowse.Server where

import           Ircbrowse.Config
import           Ircbrowse.Types
import           Ircbrowse.Controllers.Cache
import           Ircbrowse.Model.Migrations
import           Ircbrowse.Model.Data
import qualified Ircbrowse.Controllers as C

import           Control.Applicative
import           Data.Maybe
import           Database.PostgreSQL.Base   (newPool)
import           Database.PostgreSQL.Simple (Pool)
import           Snap.App
import           Snap.App.Migrate
import           Snap.Http.Server           hiding (Config)
import           Snap.Util.FileServe
import           System.Environment

-- | Run the server.
runServer :: IO ()
runServer = do
  cpath:action <- getArgs
  config <- getConfig cpath
  pool <- newPool (configPostgres config)
  let model = runDB () config pool
  case foldr const "" action of
    "create-version" -> do
      putStrLn "Running migration setup and ending."
      model $ migrate True versions
    "generate-data" -> do
      model $ generateData
      clearCache config
    _ -> do
      model $ migrate False versions
      setUnicodeLocale "en_US"
      clearCache config
      httpServe server (serve config pool)

  where server = setPort 10001 defaultConfig

-- | Serve the controllers.
serve :: Config -> Pool -> Snap ()
serve config pool = route routes where
  routes = [("/js/",serveDirectory "static/js")
           ,("/css/",serveDirectory "static/css")
           ,("/js/",serveDirectory "static/js")
           ,("/",run C.overview)
           ,("/browse/:network/:channel",run C.browse)
           ]
  run = runHandler PState config pool
