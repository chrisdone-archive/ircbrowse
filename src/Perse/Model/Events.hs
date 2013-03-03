module Perse.Model.Events where

import Perse.Types
import Perse.Data

import Data.Char
import Snap.App

getEvents :: Maybe String -> Maybe String -> Maybe UTCTime -> Pagination -> Maybe String
          -> Model c s (Pagination,Integer,[Event])
getEvents network channel timestamp pagination q = do
  (pagination',events) <- case timestamp of
    Nothing -> getSearchedEvents q pagination
    Just t -> getTimestampedEvents t pagination
  count <- single ["SELECT COUNT(*) FROM event"
                  ,"WHERE"
                  ,"(? IS NULL OR (to_tsvector('english',text) @@ to_tsquery('english',?)"
                  ,"AND type IN ('talk','act')))"]
                  (tsquery q,tsquery q)
  return (pagination',fromMaybe 0 count,events)

tsquery q = fmap (intercalate "&" . map normalize . words) q where
      normalize = filter isok
      isok c = c `elem` "-_" || isDigit c || isLetter c

getSearchedEvents q pagination = do
  events <- query ["SELECT * FROM event"
                  ,"WHERE"
                  ,"(? IS NULL OR (to_tsvector('english',text) @@ to_tsquery('english',?)"
                  ,"AND type IN ('talk','act')))"
                  ,"ORDER BY timestamp ASC"
                  ,"OFFSET ?"
                  ,"LIMIT ?"]
                  (tsquery q,tsquery q
                  ,max 0 (pnPage pagination - 1) * pnLimit pagination
                  ,pnLimit pagination)
  return (pagination,events)

getTimestampedEvents t pagination = do
  rowsBefore <- fmap (fromMaybe 0)
                     (single ["SELECT COUNT(*) FROM event"
                             ,"WHERE timestamp at time zone 'utc' < ?"]
                             (Only t))
  let pagination' = pagination { pnPage = rowsBefore `div` pnLimit pagination + 1 }
  events <- getPaginatedEvents pagination'
  return (pagination',events)

getPaginatedEvents pagination =
  query ["SELECT * FROM event"
        ,"ORDER BY timestamp ASC"
        ,"OFFSET ?"
        ,"LIMIT ?"]
        (max 0 (pnPage pagination - 1) * pnLimit pagination
        ,pnLimit pagination)
