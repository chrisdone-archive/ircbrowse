{-# LANGUAGE OverloadedStrings #-}

-- | The web server.

module Perse.Server where

import Perse.Config
import Perse.Types
import qualified Perse.Controllers as C

import Database.PostgreSQL.Base   (newPool)
import Database.PostgreSQL.Simple (Pool)
import Snap.App (Snap,runHandler,route)
import Snap.Http.Server           hiding (Config)
import Snap.Util.FileServe
import System.Environment

-- | Run the server.
runServer :: IO ()
runServer = do
  cpath:_ <- getArgs
  config <- getConfig cpath
  setUnicodeLocale "en_US"
  pool <- newPool (configPostgres config)
  httpServe server (serve config pool)
    where server = setPort 10001 defaultConfig

-- | Serve the controllers.
serve :: Config -> Pool -> Snap ()
serve conf p = route routes where
  routes = [("/js/",serveDirectory "static/js")
           ,("/css/",serveDirectory "static/css")
           ,("/js/",serveDirectory "static/js")
           ,("/",run C.home)
           ]
  run = runHandler () conf p
