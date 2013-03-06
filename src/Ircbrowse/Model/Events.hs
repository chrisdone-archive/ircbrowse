module Ircbrowse.Model.Events where

import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Monads

import Data.Char
import Data.String
import Data.Default
import Data.Text (Text)
import Data.Time
import Snap.App
import Sphinx

getEvents :: Maybe String -> Maybe String -> Maybe UTCTime -> Pagination -> Maybe Text
          -> Model c s (Int,[Event],Double,NominalDiffTime)
getEvents network channel timestamp pagination q = do
  case q of
    Just q -> do
      result <- io $ search def
        { sPath = "/opt/sphinx/bin/search"
        , sConfig = "sphinx.conf"
        , sQuery = escapeText q
        }
      case result of
        Left err -> do io $ appendFile "/tmp/sphinx-error.log" (err ++"\n")
                       return (0,[],0,0)
        Right result -> do
          now <- io getCurrentTime
          results <- getEventsByResults (map fst (rResults result))
          after <- io getCurrentTime
          return (rTotal result,results,rSecs result,diffUTCTime after now)
    Nothing -> getPaginatedEvents pagination

getEventsByResults :: [Int] -> Model c s [Event]
getEventsByResults eids = do
  query ["SELECT id,timestamp,network,channel,type,nick,text"
        ,"FROM event"
        ,"WHERE id in ("
        ,intercalate ", " (map show eids)
        ,")"]
        ()

getPaginatedEvents pagination = do
  count <- single ["SELECT COUNT(*) FROM event"] ()
  rows <- query ["SELECT id,timestamp,network,channel,type,nick,text FROM event"
                ,"ORDER BY timestamp ASC"
                ,"OFFSET ?"
                ,"LIMIT ?"]
                (max 0 (pnPage pagination - 1) * pnLimit pagination
                ,pnLimit pagination)
  return (fromMaybe 0 count,rows,0,0)
