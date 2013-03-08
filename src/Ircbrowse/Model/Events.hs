module Ircbrowse.Model.Events where

import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Monads

import Data.Text (Text)
import Snap.App
import Sphinx
import Text.Blaze.Pagination

getEvents :: Maybe String -> Maybe String -> Maybe Integer -> PN -> Maybe Text
          -> Model c s (Pagination,[Event])
getEvents network channel tid (PN _ pagination _) q = do
  case q of
    Just q -> do
      result <- io $ search def
        { sPath = "/opt/sphinx/bin/search"
        , sConfig = "sphinx.conf"
        , sQuery = escapeText q
        , sOffset = fromIntegral ((pnCurrentPage pagination - 1) * pnPerPage pagination)
        , sLimit = fromIntegral (pnPerPage pagination)
        }
      case result of
        Left err -> do io $ appendFile "/tmp/sphinx-error.log" (err ++"\n")
                       return (pagination { pnTotal = 0 },[])
        Right result -> do
          results <- getEventsByResults (map fst (rResults result))
          return (pagination { pnTotal = fromIntegral (rTotal result) }
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
        ,") ORDER BY timestamp DESC"]
        ()

getTimestampedEvents tid pagination = do
  getPaginatedEvents pagination { pnCurrentPage = tid `div` pnPerPage pagination + 1 }

getPaginatedEvents pagination = do
  count <- single ["SELECT count FROM event_count"] ()
  events <- query ["SELECT id,timestamp,network,channel,type,nick,text FROM event"
                  ,"WHERE id in ("
                  ,intercalate ", " (map show [offset .. offset + limit])
                  ,")"
                  ,"ORDER BY timestamp ASC"
                  ,"LIMIT ?"]
                  (Only limit)
  return (pagination { pnTotal = fromMaybe 0 count }
         ,events)

  where offset = 1 + (max 0 (pnCurrentPage pagination - 1) * pnPerPage pagination)
        limit = pnPerPage pagination
