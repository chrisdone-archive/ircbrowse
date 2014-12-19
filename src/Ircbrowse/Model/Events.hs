module Ircbrowse.Model.Events where

import Database.PostgreSQL.Simple.FromRow
import Ircbrowse.Data
import Ircbrowse.Monads
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Data.Text (Text)
import Snap.App
import Sphinx
import Text.Blaze.Pagination

getEvents :: Channel -> Maybe Integer -> PN -> Maybe Text
          -> Model c s (Pagination,[Event])
getEvents channel tid (PN _ pagination _) q = do
  case q of
    Just q -> do
      result <- io $ search def
        { sPath = "/opt/sphinx/bin/search"
        , sConfig = "sphinx.conf"
        , sQuery = escapeText q
        , sOffset = fromIntegral ((pnCurrentPage pagination - 1) * pnPerPage pagination)
        , sLimit = fromIntegral (pnPerPage pagination)
        , sFilters = [("channel",showChanInt channel)]
        }
      case result of
        Left{} -> return (pagination { pnTotal = 0 },[])
        Right result -> do
          results <- getEventsByIds channel (map fst (rResults result))
          return (pagination { pnTotal = fromIntegral (rTotal result) }
                 ,results)
    Nothing -> do
      case tid of
        Nothing -> getPaginatedEvents channel pagination
        Just t -> getTimestampedEvents channel t pagination

getFirstEventDate :: Channel -> Model c s Day
getFirstEventDate channel = do
  today <- fmap utctDay (io getCurrentTime)
  fmap (maybe today utctDay)
       (single ["SELECT timestamp"
               ,"FROM event"
               ,"WHERE channel = ?"
               ,"ORDER BY timestamp"
               ,"ASC LIMIT 1"]
               (Only (showChanInt channel)))

getEventsByOrderIds :: Channel -> [Int] -> Model c s [Event]
getEventsByOrderIds channel eids = do
  query ["SELECT id,timestamp,network,channel,type,nick,text"
        ,"FROM event"
        ,"WHERE id in (SELECT origin FROM event_order_index WHERE idx = ? AND id IN ("
        ,intercalate ", " (map show eids)
        ,")) ORDER BY id ASC"]
        (Only (idxNum channel))

getEventsByIds :: Channel -> [Int] -> Model c s [Event]
getEventsByIds channel eids = do
  query ["SELECT (SELECT id FROM event_order_index WHERE origin = event.id AND idx = ? limit 1),"
        ,"timestamp,network,channel,type,nick,text"
        ,"FROM event"
        ,"WHERE id IN ("
        ,intercalate ", " (map show eids)
        ,") ORDER BY id DESC"]
        (Only (idxNum channel))

getEventsByDay :: Channel -> Day -> Bool -> Model c s [Event]
getEventsByDay channel day everything =
  if everything
     then getAllEventsByDay channel day
     else getRecentEventsByDay channel day

getRecentEventsByDay :: Channel -> Day -> Model c s [Event]
getRecentEventsByDay channel _ = do
  count <- single ["SELECT count FROM event_count where channel = ?"] (Only (showChanInt channel))
  let offset = fromMaybe 0 count - limit
  query ["SELECT idx.id,e.timestamp,e.network,e.channel,e.type,e.nick,e.text FROM event e,"
         ,"event_order_index idx"
         ,"WHERE e.id = idx.origin and idx.idx = ? and idx.id >= ?"
         ,"ORDER BY e.id DESC"
         ,"LIMIT ?"]
         (idxNum channel
         ,offset
         ,limit)

  where limit = 50 :: Int

getAllEventsByDay :: Channel -> Day -> Model c s [Event]
getAllEventsByDay channel day =
  query ["SELECT (SELECT id FROM event_order_index WHERE origin = event.id AND idx = ? limit 1) as id,"
        ,"timestamp,network,channel,type,nick,text"
        ,"FROM event"
        ,"WHERE channel = ?"
        ,"AND timestamp >= ?"
        ,"AND timestamp < (?::timestamp) + interval '1 day'"
        ,"ORDER BY id ASC"
        ]
       (idxNum channel
       ,showChanInt channel
       ,day
       ,day)

getTimestampedEvents :: FromRow r
                     => Channel
                     -> Integer
                     -> Pagination
                     -> Model c s (Pagination,[r])
getTimestampedEvents channel tid pagination = do
  getPaginatedEvents channel pagination
    { pnCurrentPage = if mod tid (pnPerPage pagination) == 0
                         then tid `div` pnPerPage pagination
                         else tid `div` pnPerPage pagination + 1
    }

getPaginatedEvents :: FromRow r
                   => Channel -> Pagination -> Model c s (Pagination,[r])
getPaginatedEvents channel pagination = do
  count <- single ["SELECT count FROM event_count where channel = ?"] (Only (showChanInt channel))
  events <- query ["SELECT idx.id,e.timestamp,e.network,e.channel,e.type,e.nick,e.text FROM event e,"
                  ,"event_order_index idx"
                  ,"WHERE e.id = idx.origin and idx.idx = ? and idx.id >= ? and idx.id < ?"
                  ,"ORDER BY e.id asc"
                  ,"LIMIT ?"]
                  (idxNum channel
                  ,offset
                  ,offset + limit
                  ,limit)
  return (pagination { pnTotal = fromMaybe 0 count }
         ,events)

  where offset = 1 + (max 0 (pnCurrentPage pagination - 1) * pnPerPage pagination)
        limit = pnPerPage pagination

getPaginatedPdfs :: FromRow r
                 => Channel -> PN -> Model c s (Pagination,[r])
getPaginatedPdfs channel (PN _ pagination _) = do
  count <- single ["SELECT COUNT(*) FROM event_order_index WHERE idx = ?"]
                  (Only (idxNum channel + 1))
  events <- query ["SELECT idx.id,e.timestamp,e.network,e.channel,e.type,e.nick,e.text FROM event e,"
                  ,"event_order_index idx"
                  ,"WHERE e.id = idx.origin and idx.idx = ? and idx.id >= ? and idx.id < ?"
                  ,"ORDER BY idx.id asc"
                  ,"LIMIT ?"]
                  (idxNum channel + 1
                  ,offset
                  ,offset + limit
                  ,limit)
  return (pagination { pnTotal = fromMaybe 0 count }
         ,events)

  where offset = 1 + (max 0 (pnCurrentPage pagination - 1) * pnPerPage pagination)
        limit = pnPerPage pagination

getAllPdfs :: Channel -> Model c s [(Int,ZonedTime,Text)]
getAllPdfs channel = do
  events <- query ["SELECT e.id,e.timestamp,e.text FROM event e,"
                  ,"event_order_index idx"
                  ,"WHERE e.id = idx.origin and idx.idx = ?"
                  ,"ORDER BY idx.id asc"]
                  (Only (idxNum channel + 1))
  return events
