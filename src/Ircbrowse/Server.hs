{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The web server.

module Ircbrowse.Server where

import           Ircbrowse.Types
import qualified Ircbrowse.Controllers as C

import           Database.PostgreSQL.Simple (Pool)
import           Snap.App
import           Snap.Http.Server           hiding (Config)
import           Snap.Util.FileServe

-- | Run the server.
runServer :: Config -> Pool -> IO ()
runServer config pool = do
  setUnicodeLocale "en_US"
  httpServe server (serve config pool)

  where server = setPort 10001 defaultConfig

-- | Serve the controllers.
serve :: Config -> Pool -> Snap ()
serve config pool = route routes where
  routes = [("/js/",serveDirectory "static/js")
           ,("/css/",serveDirectory "static/css")
           ,("/js/",serveDirectory "static/js")
           ,("/browse/:network/:channel",run C.browse)
           ,("/nick-cloud",run C.nickCloud)
           ,("/social",run C.socialGraph)
           ,("/",run C.overview)
           ]
  run = runHandler PState config pool
