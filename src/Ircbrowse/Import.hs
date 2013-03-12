{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing from tunes.org.

module Ircbrowse.Import where


import           Ircbrowse.Data
import           Ircbrowse.Model.Migrations
import           Ircbrowse.Monads
import           Ircbrowse.System
import           Ircbrowse.Tunes
import           Ircbrowse.Types

import           Control.Concurrent
import qualified Data.ByteString as S
import           Data.IRC.CLog.Parse hiding (Config)
import           Data.Text (Text)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Database.PostgreSQL.Base.Types (Pool)
import           Snap.App
import           Snap.App.Migrate

-- | Do a batch import of files in the current directory.
batchImport :: Config -> Channel -> Pool -> IO ()
batchImport config channel pool = do
  files <- fmap (sort . filter (not . all (=='.'))) (getDirectoryContents ".")
  chan <- newChan
  done <- newChan
  forM_ files $ \file -> do
    runDB config () pool $ importFile channel config file
    putStrLn $ "Completed: " ++ file

-- | Import from yesterday all available channels.
importYesterday :: Config -> Pool -> IO ()
importYesterday config pool = do
  v <- newIORef []
  runDB config () pool $ do
    row <- query ["select type,text,timestamp"
    	   	 ,"from event"
		 ,"order by id"
		 ,"desc limit 1"] ()
    io $ writeIORef v row
  last <- readIORef v
  case listToMaybe last of
    Just ("log" :: Text,text,zonedTimeToUTC -> utctime)
      | T.isPrefixOf "ended" text ->
      forM_ [toEnum 0 ..] $ \channel ->
        importChannel config pool (addDays (-1) (utctDay utctime)) channel
    _ -> error "Unable to retrieve last ended log imported."

-- | Import the channel into the database of the given date.
importChannel :: Config -> Pool -> Day -> Channel -> IO ()
importChannel config pool day channel = do
  result <- downloadLog channel day
  case result of
    Left err -> error (show err)
    Right bytes -> do
      tmpdir <- getTemporaryDirectory
      let tmp = tmpdir </> unmakeDay day
      S.writeFile tmp bytes
      runDB config () pool $ migrate False versions
      runDB config () pool $ importFile channel config tmp

-- | Import from the given file.
importFile :: Channel -> Config -> FilePath -> Model c s ()
importFile channel config path = do
  events <- liftIO $ parseLog haskellConfig path
  case reverse events of
    (EventAt _ (decompose -> GenericEvent typ _ (T.concat -> text)):_)
      | show typ == "Log" && T.isPrefixOf "ended" text -> importEvents channel events
    _ -> error $ "Log is incomplete (no 'ended' entry)."

-- | Import an event.
importEvents :: Channel -> [EventAt] -> Model c s ()
importEvents channel events = do
  qins <- processQuery "INSERT INTO event (timestamp,network,channel,type,nick,text) VALUES"
                       ()
  qrows <- forM (zip [0..] events) $ \(i,event) ->
    case event of
      EventAt time (decompose -> GenericEvent typ mnick texts) -> do
        processQuery (if i == 0 then "(?,?,?,?,?,?)" else ",(?,?,?,?,?,?)")
                     (time
                     ,1::Int
                     ,showChanInt channel
                     ,map toLower (show typ)
                     ,fmap unNick mnick
                     ,T.concat texts)
      NoParse text -> do liftIO $ T.putStrLn $ mappend "Unable to import line: " text
                         return mempty
  [] :: [Only Int] <- queryProcessed (mappend qins (mconcat qrows))
  return ()


  where unNick (Nick t) = t

-- | Import failed lines of text (to be logged and debugged).
importFailed :: Text -> Model c s ()
importFailed text = return ()
