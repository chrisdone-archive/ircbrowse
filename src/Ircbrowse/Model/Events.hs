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

getEvents :: Maybe String -> Maybe String -> Maybe Integer -> Pagination -> Maybe Text
          -> Model c s (Pagination,[Event])
getEvents network channel tid pagination q = do
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
      case tid of
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

getTimestampedEvents tid pagination = do
  getPaginatedEvents pagination { pnPage = tid `div` pnLimit pagination + 1 }

getPaginatedEvents pagination = do
  count <- single ["SELECT count FROM event_count"] ()
  events <- query ["SELECT id,timestamp,network,channel,type,nick,text FROM event"
                  ,"WHERE id in ("
                  ,intercalate ", " (map show [offset .. offset + limit])
                  ,")"
                  ,"ORDER BY timestamp ASC"
                  ,"LIMIT ?"]
                  (Only limit)
  return (pagination { pnTotal = fromMaybe 0 count
                     , pnResults = fromIntegral (length events)
                     }
         ,events)

  where offset = 1 + (max 0 (pnPage pagination - 1) * pnLimit pagination)
        limit = pnLimit pagination
