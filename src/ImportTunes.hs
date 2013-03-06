{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing from tunes.org.

module Main where

import           Ircbrowse.Types
import           Ircbrowse.Config
import           Ircbrowse.Tunes
import           Ircbrowse.Model.Migrations

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString as S
import           Data.Char
import           Data.IRC.CLog.Parse hiding (Config)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Time
import           Database.PostgreSQL.Base (newPool)
import           Network.Curl
import           Snap.App
import           Snap.App.Migrate
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Locale

-- | Main entry point to start importing.
main :: IO ()
main = do
  batchImport

singleImport :: IO ()
singleImport = do
  (cpath:channel:_) <- getArgs
  config <- getConfig cpath
  now <- getCurrentTime
  case channel of
    "haskell" -> importChannel config (addDays (-1) (utctDay now)) Haskell
    _ -> error "unknown channel"

batchImport :: IO ()
batchImport = do
  (cpath:(parseChan -> Just channel):_) <- getArgs
  files <- fmap (sort . filter (not . all (=='.'))) (getDirectoryContents ".")
  config <- getConfig cpath
  pool <- newPool (configPostgres config)
  runDB config () pool $ migrate False versions
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

-- | Import the channel into the database of the given date.
importChannel :: Config -> Day -> Channel -> IO ()
importChannel config day channel = do
  result <- downloadLog channel day
  case result of
    Left err -> error (show err)
    Right bytes -> do
      tmpdir <- getTemporaryDirectory
      let tmp = tmpdir </> unmakeDay day
      S.writeFile tmp bytes
      pool <- newPool (configPostgres config)
      runDB config () pool $ migrate False versions
      runDB config () pool $ importFile channel config tmp

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
