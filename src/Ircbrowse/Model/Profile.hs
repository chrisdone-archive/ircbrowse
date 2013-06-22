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
  , nickLines :: Int
  , nickQuote :: Maybe (Text,Text)
  , nickQuotes :: [Text]
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
                  ,"REGEXP_REPLACE(text,'^@remember ([^ ]+)','')"
                  ,"FROM event"
                  ,"WHERE TYPE in ('talk','act') AND"
                  ,"(NOT ? OR (timestamp > ? AND timestamp < ?))"
                  ,"AND text LIKE '@remember %'"
                  ,"AND REGEXP_REPLACE(text,'^@remember ([^ ]+).*',E'\\\\1') = ?"
                  ,"ORDER BY timestamp DESC"]
                  (recent,from,to,nick)
  return $
    NickStats { nickHours = hours
              , nickYears = years
              , nickLines = lines
              , nickQuote = listToMaybe rquote
              , nickQuotes = nub (map (\(Only t) -> t) quotes)
              }

getNicks :: Int -> Bool -> Range -> Model c s [(Text,Int)]
getNicks n recent (Range from to) = do
  query ["SELECT nick,COUNT(*)"
        ,"FROM event"
        ,"WHERE TYPE in ('talk','act') AND"
        ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
        ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
        ,"OR text LIKE 'lambdabot: %')"
        ,"GROUP BY nick"
        ,"ORDER BY 2 desc"
        ,"LIMIT ?;"]
        (recent,from,to,n)

getNickCount :: Bool -> Range -> Model c s Int
getNickCount recent (Range from to) = do
  fmap (sum . map (\(Only x) -> x))
       (query ["SELECT DISTINCT COUNT(*)"
              ,"FROM (SELECT nick FROM event"
              ,"      WHERE (NOT ? OR (timestamp > ? AND timestamp < ?))"
              ,"      GROUP BY nick) c"]
              (recent,from,to))
