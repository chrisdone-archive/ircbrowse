{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The web server.

module Ircbrowse.Server where

import Ircbrowse.Config
import Ircbrowse.Types
import Ircbrowse.Controllers.Cache
import Ircbrowse.Model.Migrations
import qualified Ircbrowse.Controllers as C

import Database.PostgreSQL.Base   (newPool)
import Database.PostgreSQL.Simple (Pool)
import Snap.App
import Snap.App.Migrate
import Snap.Http.Server           hiding (Config)
import Snap.Util.FileServe
import System.Environment

-- | Run the server.
runServer :: IO ()
runServer = do
  cpath:((==["--create-version"]) -> create) <- getArgs
  config <- getConfig cpath
  pool <- newPool (configPostgres config)
  if create
     then do putStrLn "Running migration setup and ending."
             runDB () config pool $ migrate create versions
     else do
       runDB () config pool $ do
         migrate create versions
         _ <- exec ["update event_count set count = (select count(*) from event)"] ()
         return ()
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
