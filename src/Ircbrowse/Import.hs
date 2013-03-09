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
  writeList2Chan chan files
  forM_ [1..2] $ \_ -> forkIO $ do
    forever $ do
      file <- readChan chan
      runDB config () pool $ importFile channel config file
      writeChan done $ file
  forever $ do
    file <- readChan done
    case lookup file (zip files [1..]) of
      Just i  -> putStrLn $ "Completed: " ++ file ++ " (" ++ show i ++ "/" ++ show (length files) ++ ")"
      Nothing -> putStrLn $ "Bogus: " ++ file

-- | Import from yesterday all available channels.
importYesterday :: Config -> Pool -> IO ()
importYesterday config pool = do
  today <- fmap utctDay getCurrentTime
  forM_ [toEnum 0 ..] $ importChannel config pool (addDays (-1) today)

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
  importEvents channel events

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
