{-# LANGUAGE OverloadedStrings #-}

-- | Statistics for a specific nick's profiling.

module Ircbrowse.Model.Profile where

import Ircbrowse.Data
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Snap.App
import Data.Text (Text)
import Data.Time
import Data.Monoid

data NickStats = NickStats
  { nickHours  :: ![(Int,Int,Int,Int)]
  , nickYears  :: ![(Int,Int,Int,Int,Int)]
  , nickLines  :: !Int
  , nickQuote  :: !(Maybe (Text,Text))
  , nickQuotes :: ![(Integer,UTCTime,Text)]
  , nickKarma  :: !Int
  }

activeHours :: Text -> Bool -> Range -> Model c s NickStats
activeHours nick recent (Range from to) = do
  hours <- query ["SELECT"
                 ,"DATE_PART('HOUR',timestamp) :: integer AS hour,"
                 ,"AVG(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
                 ,"SUM(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
                 ,"COUNT(*) AS lines"
                 ,"FROM event"
                 ,"WHERE nick = ? and type in ('talk','act') AND"
                 ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
                 ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
                 ,"OR text LIKE 'lambdabot: %' OR text ~ '^[^ :]: ')"
                 ,"GROUP BY DATE_PART('HOUR',timestamp)"
                 ,"ORDER BY hour;"]
                 (nick,recent,from,to)
  years <- query ["SELECT"
                 ,"DATE_PART('YEAR',timestamp) :: integer,"
                 ,"ROUND(AVG(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1))) :: integer,"
                 ,"MAX(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
                 ,"SUM(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
                 ,"COUNT(*) :: integer"
                 ,"FROM event"
                 ,"WHERE nick = ? and TYPE in ('talk','act') AND"
                 ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
                 ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
                 ,"OR text LIKE 'lambdabot: %')"
                 ,"GROUP BY DATE_PART('YEAR',timestamp)"
                 ,"ORDER BY DATE_PART('YEAR',timestamp);"]
                 (nick,recent,from,to)
  let lines = sum (map (\(_,_,_,_,lines) -> lines) years)
  rquote <- query ["SELECT"
                  ,"type,text"
                  ,"FROM event"
                  ,"WHERE nick = ? and TYPE in ('talk','act') AND"
                  ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
                  ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
                  ,"OR text LIKE 'lambdabot: %')"
                  ,"OFFSET random()*?"
                  ,"LIMIT 1"]
                  (nick,recent,from,to,lines)
  quotes <- query ["SELECT"
                  ,"index.id,timestamp,REGEXP_REPLACE(text,'^@remember ([^ ]+)','')"
                  ,"FROM event, event_order_index index"
                  ,"WHERE TYPE in ('talk','act') AND"
                  ,"index.idx = ? AND index.origin=event.id AND"
                  ,"(NOT ? OR (timestamp > ? AND timestamp < ?))"
                  ,"AND text LIKE '@remember %'"
                  ,"AND REGEXP_REPLACE(text,'^@remember ([^ ]+).*',E'\\\\1') = ?"
                  ,"ORDER BY timestamp DESC"]
                  (idxNum Haskell,recent,from,to,nick)
  karma <- single ["select count (*) from event where text like ?"]
                  (Only (nick <> "++%"))
  return $
    NickStats { nickHours = hours
              , nickYears = years
              , nickLines = lines
              , nickQuote = listToMaybe rquote
              , nickQuotes = quotes
              , nickKarma = fromMaybe 0 karma
              }
