-- | Model for @remember'd quotes. Does not take into account @forget.

module Ircbrowse.Model.Quotes where

import Ircbrowse.Types
import Ircbrowse.Types.Import

import Data.Text
import Data.Time
import Snap.App

-- | Get the most recent n quotes.
getRecentQuotes :: Int -> Model c s [(Integer,UTCTime,Text)]
getRecentQuotes n = do
  query ["SELECT index.id,timestamp,REGEXP_REPLACE(text,'^@remember ([^ ]+)',E'<\\\\1>')"
        ,"FROM event, event_order_index index"
        ,"WHERE"
        ,"channel = ? AND"
        ,"index.idx = ? AND index.origin=event.id AND"
        ,"TYPE IN ('talk','act') AND"
        ,"text LIKE '@remember %'"
        ,"ORDER BY timestamp DESC"
        ,"LIMIT ?"]
        (showChanInt Haskell
        ,idxNum Haskell
        ,n)
