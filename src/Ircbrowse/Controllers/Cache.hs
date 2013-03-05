{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | HTML caching.

module Ircbrowse.Controllers.Cache
       (cache
       ,cacheIf
       ,resetCache
       ,clearCache
       ,resetCacheModel)
       where

import           Ircbrowse.Blaze
import           Ircbrowse.Data
import           Ircbrowse.Monads
import           Ircbrowse.System
import           Ircbrowse.Types

import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy.IO as T
import           Snap.App
import           System.FilePath

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

clearCache :: Config -> IO ()
clearCache config = do
  files <- getDirectoryContents dir
  forM_ (filter (not . all (=='.')) files) $ removeFile . (dir </>)

  where dir = configCacheDir config

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
keyToString (Overview network channel (Range from to)) =
  "overview-" ++ opt network ++ "-" ++ opt channel ++ "-" ++ showDay from ++ "-" ++ showDay to ++ ".html"
    where opt Nothing = "_"
          opt (Just x) = x
keyToString (Browse network channel utctime pagination) =
  "browse-" ++ opt network ++ "-" ++ opt channel ++ "-" ++ fromMaybe "" (fmap showTime utctime) ++
  "-page" ++ show (pnPage pagination) ++ "-of-" ++ show (pnLimit pagination) ++ ".html"
    where opt Nothing = "_"
          opt (Just x) = x

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%s"
