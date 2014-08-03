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
           ,("/nick-cloud/:channel",run C.nickCloud)
           ,("/social",run C.socialGraph)
           ,("/day/:channel/:year/:month/:day",run (C.browseDay False))
           ,("/day/:channel/today/:mode",run (C.browseDay True))
           ,("/day/:channel/today",run (C.browseDay True))
           ,("/nick/:nick",run C.nickProfile)
           ,("/nicks/:channel/:mode",run C.allNicks)
           ,("/nicks/:channel",run C.allNicks)
           ,("/quotes.rss",run C.quotes)
           ,("/pdfs/:channel/:unique",run C.pdfs)
           ,("/pdfs/:channel",run C.pdfs)
           ,("/stats/:channel",run C.stats)
           ,("/calendar/:channel",run C.calendar)
           ,("/:channel",run C.stats)
           ,("/selection/:channel",run C.browseSpecified)
           ,("/",run C.overview)
           ]
  run = runHandler PState config pool
