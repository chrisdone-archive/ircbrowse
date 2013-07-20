-- | Model for @remember'd quotes. Does not take into account @forget.

module Ircbrowse.Model.Quotes where

import Ircbrowse.Types

import Data.Text
import Data.Time
import Snap.App

-- | Get the most recent n quotes.
getRecentQuotes :: Int -> Model c s [(UTCTime,Text)]
getRecentQuotes n = do
  query ["SELECT timestamp,REGEXP_REPLACE(text,'^@remember ([^ ]+)',E'<\\\\1>')"
        ,"FROM event"
        ,"WHERE TYPE IN ('talk','act') AND"
        ,"text LIKE '@remember %'"
        ,"ORDER BY timestamp DESC"
        ,"LIMIT ?"]
        (Only n)
