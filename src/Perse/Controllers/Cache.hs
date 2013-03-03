{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | HTML caching.

module Perse.Controllers.Cache
       (cache
       ,cacheIf
       ,resetCache
       ,resetCacheModel)
       where


import           Perse.Types

import           Control.Monad
import           Control.Monad.IO         (io)
import           Control.Monad.Reader     (asks)
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy.IO as T
import           Data.Time
import           Snap.App.Types
import           System.Directory
import           System.Locale
import           Text.Blaze.Html5         (Html)
import           Text.Blaze.Renderer.Text (renderHtml)

-- | Cache conditionally.
cacheIf :: Bool -> Key -> Controller Config s (Maybe Html) -> Controller Config s (Maybe Text)
cacheIf pred key generate =
  if pred
     then cache key generate
     else fmap (fmap renderHtml) generate

-- | Generate and save into the cache, or retrieve existing from the
-- | cache.
cache :: Key -> Controller Config s (Maybe Html) -> Controller Config s (Maybe Text)
cache key generate = do
  tmpdir <- asks (configCacheDir . controllerStateConfig)
  let cachePath = tmpdir ++ "/" ++ keyToString key
  exists <- io $ doesFileExist cachePath
  if exists
     then do text <- io $ T.readFile cachePath
     	     return (Just text)
     else do text <- fmap (fmap renderHtml) generate
     	     case text of
	       Just text' -> do io $ createDirectoryIfMissing True tmpdir
                                io $ T.writeFile cachePath text'
	       	    	        return text
               Nothing -> return text

-- | Reset an item in the cache.
resetCache :: Key -> Controller Config s ()
resetCache key = do
  tmpdir <- asks (configCacheDir . controllerStateConfig)
  io $ do
   let cachePath = tmpdir ++ "/" ++ keyToString key
   exists <- io $ doesFileExist cachePath
   when exists $ removeFile cachePath

-- | Reset an item in the cache.
resetCacheModel :: Key -> Model Config s ()
resetCacheModel key = do
  tmpdir <- asks (configCacheDir . modelStateConfig)
  io $ do
   let cachePath = tmpdir ++ "/" ++ keyToString key
   exists <- io $ doesFileExist cachePath
   when exists $ removeFile cachePath

keyToString :: Key -> String
keyToString (Home (Range from to)) = "home-" ++ showDay from ++ "-" ++ showDay to ++ ".html"

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"
