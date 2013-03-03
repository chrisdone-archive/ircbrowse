module Perse.Model.Events where

import Perse.Types
import Perse.Data

import Snap.App

getEvents :: Maybe String -> Maybe String -> Range -> Pagination -> Model c s (Integer,[Event])
getEvents network channel (Range from to) pagination = do
  events <- query ["SELECT * FROM event"
                  ,"WHERE timestamp > ? and timestamp < ?"
                  ,"ORDER BY timestamp ASC"
                  ,"OFFSET ?"
                  ,"LIMIT ?"]
                  (from
                  ,to
                  ,max 0 (pnPage pagination - 1) * pnLimit pagination
                  ,pnLimit pagination)
  count <- single ["SELECT COUNT(*) FROM event"
                  ,"WHERE timestamp > ? and timestamp < ?"]
                  (from
                  ,to)
  return (fromMaybe 0 count,events)
