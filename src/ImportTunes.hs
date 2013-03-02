{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing from tunes.org.

module Main where

import Perse.Types
import Perse.Config
import Perse.Tunes
import Perse.Model.Migrations

import Control.Monad.Trans
import Control.Monad
import qualified Data.ByteString as S
import Data.Char
import Data.Monoid
import Data.IRC.CLog.Parse hiding (Config)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time
import Database.PostgreSQL.Base (newPool)
import Network.Curl
import Snap.App
import Snap.App.Migrate
import System.Directory
import System.Environment
import System.FilePath
import System.Locale

-- | Main entry point to start importing.
main :: IO ()
main = do
  (cpath:channel:_) <- getArgs
  config <- getConfig cpath
  now <- getCurrentTime
  case channel of
    "haskell" -> importChannel config (addDays (-1) (utctDay now)) Haskell
    _ -> error "unknown channel"

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
      events <- parseLog haskellConfig tmp
      pool <- newPool (configPostgres config)
      runDB config () pool $ migrate False versions
      runDB config () pool $ importEvents channel events

-- | Import an event.
importEvents :: Channel -> [EventAt] -> Model c s ()
importEvents channel events = do
  qins <- processQuery "INSERT INTO event (timestamp,network,channel,type,nick,text) VALUES"
                       ()
  qrows <- forM (zip [0..] events) $ \(i,EventAt time (decompose -> GenericEvent typ mnick texts)) -> do
    processQuery (if i == 0 then "(?,?,?,?,?,?)" else ",(?,?,?,?,?,?)")
                 (time
                 ,"freenode" :: Text
                 ,showChan channel
                 ,map toLower (show typ)
                 ,fmap unNick mnick
                 ,T.concat texts)
  [] :: [Only Int] <- queryProcessed (mappend qins (mconcat qrows))
  return ()


  where unNick (Nick t) = t

-- | Import failed lines of text (to be logged and debugged).
importFailed :: Text -> Model c s ()
importFailed text = return ()
