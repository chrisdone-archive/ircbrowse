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
          -> Model c s (Pagination,[Event])
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
                       return (pagination { pnResults = 0 , pnTotal = 0 },[])
        Right result -> do
          results <- getEventsByResults (map fst (rResults result))
          return (pagination { pnResults = fromIntegral (rReturned result)
                             , pnTotal = fromIntegral (rTotal result)
                             }
                 ,results)
    Nothing -> do
      case timestamp of
        Nothing -> getPaginatedEvents pagination
        Just t -> getTimestampedEvents t pagination

getEventsByResults :: [Int] -> Model c s [Event]
getEventsByResults eids = do
  query ["SELECT id,timestamp,network,channel,type,nick,text"
        ,"FROM event"
        ,"WHERE id in ("
        ,intercalate ", " (map show eids)
        ,")"]
        ()

getTimestampedEvents t pagination = do
  rowsBefore <- fmap (fromMaybe 0)
                     (single ["SELECT COUNT(*) FROM event"
                             ,"WHERE timestamp at time zone 'utc' < ?"]
                             (Only t))
  getPaginatedEvents pagination { pnPage = rowsBefore `div` pnLimit pagination + 1 }

getPaginatedEvents pagination = do
  count <- single ["SELECT count FROM event_count"] ()
  events <- query ["SELECT id,timestamp,network,channel,type,nick,text FROM event"
                  ,"WHERE id >= ? and id <= ?"
                  ,"ORDER BY timestamp ASC"
                  ,"LIMIT ?"]
                  (offset
                  ,offset + limit
                  ,limit)
  return (pagination { pnTotal = fromMaybe 0 count
                     , pnResults = fromIntegral (length events)
                     }
         ,events)

  where offset = 1 + (max 0 (pnPage pagination - 1) * pnLimit pagination)
        limit = pnLimit pagination
