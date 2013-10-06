{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing from logs.

module Ircbrowse.Import where

import           Ircbrowse.Data
import           Ircbrowse.Model.Data
import           Ircbrowse.Model.Migrations
import           Ircbrowse.Monads
import           Ircbrowse.System
import           Ircbrowse.Types
import           Ircbrowse.Types.Import

import           Control.Concurrent
import qualified Data.ByteString as S
import           Data.IRC.CLog.Parse hiding (Config,parseLog)
import           Data.IRC.Znc.Parse hiding (Config)
import           Data.Text (Text)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Snap.App
import           Snap.App.Migrate

-- -- | Do a batch import of files in the current directory.
-- batchImport :: Config -> Channel -> Pool -> IO ()
-- batchImport config channel pool = do
--   files <- fmap (sort . filter (not . all (=='.'))) (getDirectoryContents ".")
--   hSetBuffering stdout NoBuffering
--   forM_ files $ \file -> do
--     putStr $ "Importing " ++ file ++ " ... "
--     runDB config () pool $ importFile channel config file
--     putStrLn $ "Done."
--     removeFile file

-- | Import from yesterday all available channels.
importYesterday :: Config -> Pool -> IO ()
importYesterday config pool = do
  forM_ [Haskell,Lisp,HaskellGame] $ \channel -> do
    putStrLn $ "Importing channel: " ++ showChan channel
    v <- newIORef []
    runDB config () pool $ do
      row <- query ["select timestamp"
		   ,"from event"
		   ,"where channel = ?"
		   ,"order by id"
		   ,"desc limit 1"]
                   (Only (showChanInt channel))
      io $ writeIORef v row
    last <- readIORef v
    case listToMaybe last of
      Just event@(Only (lastdate::UTCTime)) -> do
        now <- getCurrentTime
        let today = utctDay now
        putStrLn $ "Last date: " ++ show lastdate
        putStrLn $ "Importing from day: " ++ show (utctDay lastdate)
        when (utctDay lastdate /= today) $
          putStrLn $ "And also day: " ++ show (addDays 1 (utctDay lastdate))
        importChannel lastdate config pool (utctDay lastdate) channel
        when (utctDay lastdate /= today) $
          importChannel lastdate config pool (addDays 1 (utctDay lastdate)) channel
        putStrLn $ "Imported channel " ++ showChan channel
      _ -> do
        logs <- fmap (sort) (getDirectoryContents (configLogDir config))
        case find (isInfixOf ("#" ++ showChan channel ++ "_")) logs of
          Nothing -> error $ "Unable to get last import time, or find the first log of this channel: " ++ showChan channel
          Just fp ->
            case parseFileTime fp of
              Nothing -> error $ "Found log of this channel in the log dir, but couldn't parse date from the filename: " ++ fp
              Just lastdate -> do
                putStrLn $ "This channel has never been imported: " ++ showChan channel
                putStrLn $ "Going to import from: " ++ show (lastdate :: UTCTime)
                importChannel lastdate config pool (utctDay lastdate) channel
                return ()
  putStrLn "Running ANALYZE ..."
  runDB config () pool $ void $ exec ["ANALYZE event_order_index"] ()
  putStrLn "Running data regeneration ..."
  runDB config () pool $ generateData

parseFileTime (drop 1 . dropWhile (/='_') -> date) =
  parseTime defaultTimeLocale "%Y%m%d.log" date

-- | Import the channel into the database of the given date.
importChannel :: UTCTime -> Config -> Pool -> Day -> Channel -> IO ()
importChannel last config pool day channel = do
  tmp <- copyLog channel day
  let db = runDB config () pool
  db $ migrate False versions
  db $ importFile last channel config tmp
  updateChannelIndex config pool channel
  removeFile tmp

  where copyLog chan day = do
          let fp = "#" ++ showChan chan ++ "_" ++ unmakeDay day ++ ".log"
          tmp <- getTemporaryDirectory
          putStrLn $ "Importing from file " ++ fp
          let tmpfile = tmp </> fp
          copyFile (configLogDir config ++ fp)
                   tmpfile
          return tmpfile

unmakeDay :: FormatTime t => t -> String
unmakeDay = formatTime defaultTimeLocale "%Y%m%d"

-- | Update the event order index for the given channel.
updateChannelIndex :: Config -> Pool -> Channel -> IO ()
updateChannelIndex config pool channel = runDB config () pool $ do
  io $ putStrLn $ "Updating order indexes for " ++ showChan channel ++ " ..."
  result <- query ["SELECT id,origin"
                  ,"FROM event_order_index"
                  ,"WHERE idx = ?"
                  ,"ORDER BY id DESC"
                  ,"LIMIT 1"]
                  (Only (idxNum channel))
  case result of
    -- [] -> error $ "Unable to get information for updating the channel index: " ++ showChan channel
    [] -> void $ exec ["INSERT INTO event_order_index"
                     ,"SELECT ? + RANK() OVER(ORDER BY id ASC) AS id,"
                     ,"id AS ORIGIN,"
                     ,"? AS idx"
                     ,"FROM event"
                     ,"WHERE channel = ?"
                     ,"AND id > ?"
                     ,"ORDER BY id ASC;"]
                     (0::Int
                     ,idxNum channel
                     ,showChanInt channel
                     ,0::Int)
    ((lastId::Int,lastOrigin::Int):_) -> void $
      exec ["INSERT INTO event_order_index"
	   ,"SELECT ? + RANK() OVER(ORDER BY id ASC) AS id,"
	   ,"id AS ORIGIN,"
	   ,"? AS idx"
	   ,"FROM event"
	   ,"WHERE channel = ?"
	   ,"AND id > ?"
	   ,"ORDER BY id ASC;"]
	   (lastId
	   ,idxNum channel
	   ,showChanInt channel
	   ,lastOrigin)

-- | Import from the given file.
importFile :: UTCTime -> Channel -> Config -> FilePath -> Model c s ()
importFile last channel config path = do
   events' <- liftIO $ parseLog ircbrowseConfig path
   io $ putStrLn "Importing the following events:"
   let events = dropWhile (not.(\ (EventAt t _) -> t > last)) events'
   io $ forM_ events $ \event ->
     print event
   importEvents channel events

  -- This code is no longer applicable for ZNC. It was for tunes.org logs.
  --
  -- case reverse events of
  --   (EventAt _ (decompose -> GenericEvent typ _ (T.concat -> text)):_)
  --     | show typ == "Log" && T.isPrefixOf "ended" text -> importEvents channel events
  --   _ -> do liftIO $ removeFile path
  --     	    error $ "Log is incomplete (no 'ended' entry). (Removed file cache.)"

-- | Import an event.
importEvents :: Channel -> [EventAt] -> Model c s ()
importEvents channel events = do
   forM_ (nub events) $ \event ->
    case event of
      EventAt time (decompose -> GenericEvent typ mnick texts) -> do
        let text = T.concat texts
        unless (T.null (T.strip text)) $ void $
          exec ["INSERT INTO event (timestamp,network,channel,type,nick,text) VALUES"
	       ,"(?,?,?,?,?,?)"]
               (time
	       ,1::Int
	       ,showChanInt channel
	       ,map toLower (show typ)
	       ,fmap unNick mnick
	       ,text)
      NoParse text -> do liftIO $ T.putStrLn $ mappend "Unable to import line: " text
                         return mempty

  where unNick (Nick t) = t
