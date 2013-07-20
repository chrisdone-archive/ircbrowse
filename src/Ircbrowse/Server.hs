{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The web server.

module Ircbrowse.Server where

import           Ircbrowse.Types
import qualified Ircbrowse.Controllers as C

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
           ,("/browse/:channel",run C.browse)
           ,("/nick-cloud",run C.nickCloud)
           ,("/social",run C.socialGraph)
           ,("/nick/:nick",run C.nickProfile)
           ,("/nicks",run C.allNicks)
	   ,("/quotes.rss",run C.quotes)
           ,("/",run C.overview)
           ]
  run = runHandler PState config pool
