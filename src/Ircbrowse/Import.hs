{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing from tunes.org.

module Ircbrowse.Import where


import           Ircbrowse.Data
import           Ircbrowse.Model.Data
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
  hSetBuffering stdout NoBuffering
  forM_ files $ \file -> do
    putStr $ "Importing " ++ file ++ " ... "
    runDB config () pool $ importFile channel config file
    putStrLn $ "Done."
    removeFile file

-- | Import from yesterday all available channels.
importYesterday :: Config -> Pool -> IO ()
importYesterday config pool = do
  forM_ [Haskell,Lisp,OCaml,Scheme] $ \channel -> do
    putStrLn $ "Importing channel: " ++ showChan channel
    v <- newIORef []
    runDB config () pool $ do
      row <- query ["select type,text"
		   ,"from event"
		   ,"where channel = ?"
		   ,"order by id"
		   ,"desc limit 1"]
                   (Only (showChanInt channel))
      io $ writeIORef v row
    last <- readIORef v
    case listToMaybe last of
      Just event@("log" :: Text,text)
	| T.isPrefixOf "ended" text -> do
	case parseTunesDay (T.unpack (T.drop 1 (T.dropWhile (/='/') text))) of
	  Nothing -> error $ "Unable to parse last ended date."
	  Just lastdate -> do
	    importChannel config pool (addDays 1 lastdate) channel
            putStrLn $ "Imported channel " ++ showChan channel
      _ -> error "Unable to retrieve last ended log imported."
  putStrLn "Running ANALYZE ..."
  runDB config () pool $ void $ exec ["ANALYZE event_order_index"] ()
  putStrLn "Running data regeneration ..."
  runDB config () pool $ generateData

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
      let db = runDB config () pool
      db $ migrate False versions
      db $ importFile channel config tmp
      updateChannelIndex config pool channel

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
    [] -> error $ "Unable to get information for updating the channel index: " ++ showChan channel
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
  qrows <- forM (zip [0..] (nub events)) $ \(i,event) ->
    case event of
      EventAt time (decompose -> GenericEvent typ mnick texts) -> do
        let text = T.concat texts
        if T.null (T.strip text)
           then return mempty
           else processQuery (if i == 0 then "(?,?,?,?,?,?)" else ",(?,?,?,?,?,?)")
                             (time
                             ,1::Int
                             ,showChanInt channel
                             ,map toLower (show typ)
                             ,fmap unNick mnick
                             ,text)
      NoParse text -> do liftIO $ T.putStrLn $ mappend "Unable to import line: " text
                         return mempty
  [] :: [Only Int] <- queryProcessed (mappend qins (mconcat qrows))
  return ()


  where unNick (Nick t) = t

-- | Import failed lines of text (to be logged and debugged).
importFailed :: Text -> Model c s ()
importFailed text = return ()
