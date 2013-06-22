-- | Statistics for a specific nick's profiling.

module Ircbrowse.Model.Profile where

import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Tunes

import Snap.App
import Data.Text (Text)

data NickStats = NickStats
  { nickHours :: [(Int,Int,Int,Int)]
  , nickYears :: [(Int,Int,Int,Int,Int)]
  }

activeHours :: Text -> Bool -> Range -> Model c s NickStats
activeHours nick recent (Range from to) = do
  hours <- query ["SELECT"
                 ,"DATE_PART('HOUR',timestamp) :: integer AS hour,"
                 ,"AVG(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
                 ,"SUM(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
                 ,"COUNT(*) AS lines"
                 ,"FROM event"
                 ,"WHERE nick = ? and type = 'talk' AND"
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
                 ,"WHERE nick = ? and TYPE = 'talk' AND"
                 ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
                 ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
                 ,"OR text LIKE 'lambdabot: %')"
                 ,"GROUP BY DATE_PART('YEAR',timestamp)"
                 ,"ORDER BY DATE_PART('YEAR',timestamp);"]
                 (nick,recent,from,to)
  return $
    NickStats { nickHours = hours
              , nickYears = years
              }
