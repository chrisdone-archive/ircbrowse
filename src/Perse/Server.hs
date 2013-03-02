{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The web server.

module Perse.Server where

import Perse.Config
import Perse.Types
import Perse.Model.Migrations
import qualified Perse.Controllers as C

import Database.PostgreSQL.Base   (newPool)
import Database.PostgreSQL.Simple (Pool)
import Snap.App (Snap,runHandler,route,runDB)
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
       setUnicodeLocale "en_US"
       httpServe server (serve config pool)
    where server = setPort 10001 defaultConfig

-- | Serve the controllers.
serve :: Config -> Pool -> Snap ()
serve config pool = route routes where
  routes = [("/js/",serveDirectory "static/js")
           ,("/css/",serveDirectory "static/css")
           ,("/js/",serveDirectory "static/js")
           ,("/",run C.home)
           ]
  run = runHandler () config pool
